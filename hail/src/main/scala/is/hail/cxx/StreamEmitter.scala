package is.hail.cxx

import is.hail.expr.types.physical._
import is.hail.cxx.{ArgumentPack => AP}

object StagedParameterizedStream {
  def id[A](_fb: FunctionBuilder, pack: ArgumentPack[A]): StagedParameterizedStream[A, A] =
    new StagedParameterizedStream[A, A](_fb) {
      type S = Code
      val paramPack = pack
      val eltPack = pack
      val statePack = AP.boolean

      def init(a: A, start: S => Code, stop: Code, missing: Code) =
        start("true")

      def step(a: A, isFirst: S, cons: (A, S) => Code, stop: Code) =
        s"""
           |if ($isFirst) {
           |  ${cons(a, "false")}
           |} else {
              $stop
           |}
         """.stripMargin

      override def consumeRaw[T](a: A, pack: ArgumentPack[T],
        zero: T,
        oper: (T, A, S, (T => Code)) => Code,
        eos: T => Code, missing: Code
      ) = oper(zero, a, "false", eos)
    }

  def range(_fb: FunctionBuilder) = new StagedParameterizedStream[Code, Code](_fb) {
    type S = Code
    val paramPack = AP.int32
    val eltPack = AP.int32
    val statePack = AP.int32

    def init(len: Code, start: Code => Code, stop: Code, missing: Code): Code =
      s"""
         |if ($len > 0) {
         |  ${start("0")}
         |} else {
         |  $stop
         |}
       """.stripMargin

    def step(len: Code, i: Code, cons: (Code, Code) => Code, stop: Code): Code =
      s"""
         |if ($i < $len) {
         |  ${cons(i, s"($i + 1)")}
         |} else {
         |  $stop
         |}
       """.stripMargin

    override def consumeRaw[T](
      len: Code,
      pack: ArgumentPack[T],
      zero: T,
      oper: (T, Code, S, (T => Code)) => Code,
      eos: T => Code, missing: Code
    ): Code = {
      val lb = new LabelBuilder(fb)
      val loop = lb.label("range_loop", AP.tuple2(pack, AP.int32))
      lb.define(loop) { case (acc, i) =>
        step(len, i,
          (elt, s) => oper(acc, elt, s, acc => loop((acc, s))),
          eos(acc))
      }
      Code(lb.end(), loop((zero, "0")))
    }
  }
}

abstract class StagedParameterizedStream[P, A](val fb: FunctionBuilder) { self =>
  type S
  def paramPack: ArgumentPack[P]
  def eltPack: ArgumentPack[A]
  def statePack: ArgumentPack[S]

  def init(
    param: P,
    start: S => Code,
    stop: Code,
    missing: Code
  ): Code

  def step(
    param: P,
    state: S,
    cons: (A, S) => Code,
    stop: Code
  ): Code

  def consumeRaw[T](
    param: P,
    pack: ArgumentPack[T],
    zero: T,
    oper: (T, A, S, (T => Code)) => Code,
    eos: T => Code,
    missing: Code
  ): Code = {
    val lb = new LabelBuilder(fb)
    val loop = lb.label("loop", AP.tuple2(pack, statePack))
    lb.define(loop) { case (acc, s) =>
      step(param, s,
        (elt, s) => oper(acc, elt, s, acc => loop((acc, s))),
        eos(acc))
    }
    init(param,
      s0 => {
        s"""{
           |  ${lb.end()}
           |  ${loop((zero, s0))}
           |}
         """.stripMargin
      },
      eos(zero),
      missing)
  }

  def map[B](newEltPack: ArgumentPack[B], f: A => B) =
    new StagedParameterizedStream[P, B](fb) {
      type S = self.S
      def paramPack = self.paramPack
      val eltPack = newEltPack
      def statePack = self.statePack

      def init(param: P, start: S => Code, stop: Code, missing: Code): Code =
        self.init(param, start, stop, missing)

      def step(param: P, state: S, cons: (B, S) => Code, stop: Code): Code =
        self.step(param, state, (a, s) => cons(f(a), s), stop)

      override def consumeRaw[T](param: P, pack: ArgumentPack[T],
        zero: T,
        oper: (T, B, S, (T => Code)) => Code,
        eos: T => Code, missing: Code
      ): Code =
        self.consumeRaw[T](param, pack, zero,
          (t, a, s, k) => oper(t, f(a), s, k),
          eos, missing)
    }

  def guard[Q](
    newParamPack: ArgumentPack[Q],
    f: Q => (Code, Code, P)
  ): StagedParameterizedStream[Q, A] =
    new StagedParameterizedStream[Q, A](fb) {
      type S = (self.S, P)
      val paramPack = newParamPack
      def eltPack = self.eltPack
      def statePack = AP.tuple2(self.statePack, self.paramPack)

      def init(q: Q, start: S => Code, stop: Code, missing: Code): Code = {
        val (setup, m, _param) = f(q)
        val (psetup, param) = self.paramPack.memoize(fb, _param, "p")
        s"""
           |$setup
           |if ($m) {
           |  $missing
           |} else {
           |  $psetup
           |  ${self.init(param, s => start((s, param)), stop, missing)}
           |}
         """.stripMargin
      }

      def step(q: Q, state: S, cons: (A, S) => Code, stop: Code): Code = {
        val (s, param) = state
        self.step(param, s, (a, s) => cons(a, (s, param)), stop)
      }

      override def consumeRaw[T](q: Q, pack: ArgumentPack[T],
        zero: T,
        oper: (T, A, S, (T => Code)) => Code,
        eos: T => Code, missing: Code
      ): Code = {
        val (setup, m, _param) = f(q)
        val (psetup, param) = self.paramPack.memoize(fb, _param, "p")
        s"""
           |$setup
           |if ($m) {
           |  $missing
           |} else {
           |  $psetup
           |  ${self.consumeRaw[T](param, pack, zero,
                 (t, a, s, k) => oper(t, a, (s, param), k),
                 eos, missing)}
           |}
         """.stripMargin
      }
    }

  def zip[B](other: StagedParameterizedStream[P, B]): StagedParameterizedStream[P, (A, B)] = {
    type S1 = self.S
    type S2 = other.S
    assert(self.fb eq other.fb)
    new StagedParameterizedStream[P, (A, B)](fb) {
      type S = (S1, S2)
      def paramPack = self.paramPack
      def eltPack = AP.tuple2(self.eltPack, other.eltPack)
      def statePack = AP.tuple2(self.statePack, other.statePack)

      def init(param: P, start: ((S1,S2)) => Code, stop: Code, missing: Code): Code =
        self.init(param, s1 => {
          other.init(param, s2 => {
            start((s1, s2))
          }, stop, missing)
        }, stop, missing)

      def step(param: P, state: (S1,S2), cons: ((A,B), (S1,S2)) => Code, stop: Code): Code =
        self.step(param, state._1, (a, s1) => {
          other.step(param, state._2, (b, s2) => {
            cons((a, b), (s1, s2))
          }, stop)
        }, stop)

      override def consumeRaw[T](param: P, pack: ArgumentPack[T],
        zero: T,
        oper: (T, (A,B), (S1,S2), (T => Code)) => Code,
        eos: T => Code, missing: Code
      ): Code =
        other.init(param, s2 => {
          // self "drives" the consume loop; TODO: choose optimal loop driver
          self.consumeRaw[(T, S2)](param,
            AP.tuple2(pack, other.statePack),
            (zero, s2),
            (stuff, a, s1, continue) => {
              val (acc, s2) = stuff
              other.step(param, s2,
                (b, s2) => oper(acc, (a, b), (s1, s2), acc => continue((acc, s2))),
                eos(acc))
            },
            { case (acc, _) => eos(acc) },
            missing)
        },
          eos(zero), missing)
    }
  }

  def >>>[B](inner: StagedParameterizedStream[A, B]): StagedParameterizedStream[P, B] = {
    assert(self.fb eq inner.fb)
    new StagedParameterizedStream[P, B](fb) {
      type S = (self.S, A, inner.S)
      def paramPack = self.paramPack
      def eltPack = inner.eltPack
      def statePack = AP.tuple3(self.statePack, self.eltPack, inner.statePack)

      def init(param: P, start: S => Code, stop: Code, missing: Code): Code =
        self.consumeRaw[Unit](param, AP.unit, (),
          (_, outerElt, outerS, continue) => {
            inner.init(outerElt,
              innerS => start((outerS, outerElt, innerS)),
              continue(()),
              continue(()))
          },
          (_ => stop),
          missing)

      def step(param: P, state: S, cons: (B, S) => Code, stop: Code) = {
        val lb = new LabelBuilder(fb)
        val innerLoop = lb.label("inner_loop", AP.tuple3(self.statePack, self.eltPack, inner.statePack))
        val outerLoop = lb.label("outer_loop", self.statePack)
        lb.define(outerLoop) { outerS =>
          self.step(param, outerS,
            (outerElt, outerS) => {
              inner.init(outerElt, // 2nd call to 'inner.init()' (between init/step)
                innerS => innerLoop((outerS, outerElt, innerS)),
                outerLoop(outerS),
                outerLoop(outerS))
            },
            stop)
        }
        lb.define(innerLoop) { case (outerS, outerElt, innerS) =>
          inner.step(outerElt, innerS,
            (innerElt, innerS) => cons(innerElt, (outerS, outerElt, innerS)),
            outerLoop(outerS))
        }
        Code(
          lb.end(),
          innerLoop(state))
      }

      override def consumeRaw[T](param: P, pack: ArgumentPack[T],
        zero: T,
        oper: (T, B, S, T => Code) => Code,
        eos: T => Code, missing: Code
      ): Code =
        self.consumeRaw[T](param, pack, zero,
          (acc, outerElt, outerS, continue) => {
            inner.consumeRaw[T](outerElt, pack, acc,
              (acc, innerElt, innerS, k) => {
                oper(acc, innerElt, (outerS, outerElt, innerS), k)
              },
              continue,
              continue(acc))
          },
          eos,
          missing)
    }
  }

  def apply(param: P) =
    StagedStream(self.guard(AP.unit, _ => ("", "false", param)))
}


object StagedStream {
  def range(fb: FunctionBuilder) =
    StagedParameterizedStream.range(fb)
}

case class StagedStream[A](
  stream: StagedParameterizedStream[Unit, A]
) {

  def fb = stream.fb

  def consume[T](
    pack: ArgumentPack[T],
    zero: T,
    oper: (T, A, (T => Code)) => Code,
    eos: T => Code,
    missing: Code
  ) =
    stream.consumeRaw[T]((), pack, zero, (a, e, s, k) => oper(a, e, k), eos, missing)

  def fold[T](pack: ArgumentPack[T], zero: T)(oper: (T, A) => T): (Code, Code, T) = {
    val exit = EmitLabel(fb, AP.boolean, "exit", "m")
    val m = exit.args
    val v = pack.variables(fb, "v")
    val setup = Code(
      m.define,
      v.define,
      consume[T](pack, zero,
        (acc, elt, k) => k(oper(acc, elt)),
        acc => Code(v.store(acc), exit("false")),
        exit("true")),
      s"$exit:")
    (setup, m.load, v.load)
  }

  def map[B](newEltPack: ArgumentPack[B])(f: A => B) =
    StagedStream[B](stream.map(newEltPack, f))

  def zip[B](other: StagedStream[B]) =
    StagedStream[(A,B)](stream zip other.stream)

  def flatMap[B](f: StagedParameterizedStream[A, B]) =
    StagedStream[B](stream >>> f)
}
