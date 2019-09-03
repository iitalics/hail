package is.hail.cxx

import is.hail.expr.types.physical._
import is.hail.cxx.{ArgumentPack => AP}

abstract class StagedParameterizedStream[P, A](val fb: FunctionBuilder) { self =>
  type S
  def paramPack: ArgumentPack[P]
  def eltPack: ArgumentPack[A]
  def statePack: ArgumentPack[S]

  def init(
    param: P,
    start: S => Code,
    stop: Code
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
    eos: T => Code
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
      eos(zero))
  }

  def dimap[Q, B](newParamPack: ArgumentPack[Q], newEltPack: ArgumentPack[B], f: Q => P, g: A => B) =
    new StagedParameterizedStream[Q, B](fb) {
      type S = self.S
      val paramPack = newParamPack
      val eltPack = newEltPack
      def statePack = self.statePack

      def init(param: Q, start: S => Code, stop: Code): Code =
        self.init(f(param), start, stop)

      def step(param: Q, state: S, cons: (B, S) => Code, stop: Code): Code =
        self.step(f(param), state, (a, s) => cons(g(a), s), stop)

      override def consumeRaw[T](param: Q, pack: ArgumentPack[T],
        zero: T,
        oper: (T, B, S, (T => Code)) => Code,
        eos: T => Code
      ): Code =
        self.consumeRaw[T](f(param), pack, zero,
          (t, a, s, k) => oper(t, g(a), s, k),
          eos)
    }

  def map[B](newEltPack: ArgumentPack[B], f: A => B): StagedParameterizedStream[P, B] =
    dimap[P, B](paramPack, newEltPack, identity, f)

  def mapParam[Q](newParamPack: ArgumentPack[Q], f: Q => P): StagedParameterizedStream[Q, A] =
    dimap[Q, A](newParamPack, eltPack, f, identity)

  def zip[B](other: StagedParameterizedStream[P, B]): StagedParameterizedStream[P, (A, B)] = {
    type E1 = A
    type E2 = B
    type S1 = self.S
    type S2 = other.S
    assert(self.fb eq other.fb)
    new StagedParameterizedStream[P, (A, B)](fb) {
      type S = (S1, S2)
      def paramPack = self.paramPack
      def eltPack = AP.tuple2(self.eltPack, other.eltPack)
      def statePack = AP.tuple2(self.statePack, other.statePack)

      def init(param: P, start: ((S1,S2)) => Code, stop: Code): Code =
        self.init(param, s1 => {
          other.init(param, s2 => {
            start((s1, s2))
          }, stop)
        }, stop)

      def step(param: P, state: (S1,S2), cons: ((A,B), (S1,S2)) => Code, stop: Code): Code =
        self.step(param, state._1, (a, s1) => {
          other.step(param, state._2, (b, s2) => {
            cons((a, b), (s1, s2))
          }, stop)
        }, stop)

      override def consumeRaw[T](param: P, pack: ArgumentPack[T],
        zero: T,
        oper: (T, (A,B), (S1,S2), (T => Code)) => Code,
        eos: T => Code
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
            { case (acc, _) => eos(acc) })
        },
          eos(zero))
    }
  }

  def >>>[B](inner: StagedParameterizedStream[A, B]): StagedParameterizedStream[P, B] = {
    assert(self.fb eq inner.fb)
    new StagedParameterizedStream[P, B](fb) {
      type S = (self.S, A, inner.S)
      def paramPack = self.paramPack
      def eltPack = inner.eltPack
      def statePack = AP.tuple3(self.statePack, self.eltPack, inner.statePack)

      def init(param: P, start: S => Code, stop: Code): Code =
        self.consumeRaw[Unit](param, AP.unit, (),
          (_, outerElt, outerS, continue) => {
            inner.init(outerElt,
              innerS => start((outerS, outerElt, innerS)),
              continue(()))
          },
          (_ => stop))

      def step(param: P, state: S, cons: (B, S) => Code, stop: Code) = {
        val lb = new LabelBuilder(fb)
        val innerLoop = lb.label("inner_loop", AP.tuple3(self.statePack, self.eltPack, inner.statePack))
        val outerLoop = lb.label("outer_loop", self.statePack)
        lb.define(outerLoop) { outerS =>
          self.step(param, outerS,
            (outerElt, outerS) => {
              inner.init(outerElt, // 2nd call to 'inner.init()' (between init/step)
                innerS => innerLoop((outerS, outerElt, innerS)),
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
        eos: T => Code
      ): Code =
        self.consumeRaw[T](param, pack, zero,
          (acc, outerElt, outerS, continue) => {
            inner.consumeRaw[T](outerElt, pack, acc,
              (acc, innerElt, innerS, k) => {
                oper(acc, innerElt, (outerS, outerElt, innerS), k)
              },
              continue)
          },
          eos)
    }
  }

  def apply(param: P) =
    StagedStream(self.mapParam(AP.unit, (_ => param)))
}

class StagedRangeStream(_fb: FunctionBuilder) extends StagedParameterizedStream[Code, Code](_fb) {
  type S = Code
  val paramPack = AP.int32
  val eltPack = AP.int32
  val statePack = AP.int32

  def init(len: Code, start: Code => Code, stop: Code): Code =
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
    eos: T => Code
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


object StagedStream {
  def range(fb: FunctionBuilder) = new StagedRangeStream(fb)
}

case class StagedStream[A](
  stream: StagedParameterizedStream[Unit, A]
) {

  def fb = stream.fb

  def consume[T](
    pack: ArgumentPack[T],
    zero: T,
    oper: (T, A, (T => Code)) => Code,
    eos: T => Code
  ) =
    stream.consumeRaw[T]((), pack, zero, (a, e, s, k) => oper(a, e, k), eos)

  def map[B](newEltPack: ArgumentPack[B])(f: A => B) =
    StagedStream[B](stream.map(newEltPack, f))

  def zip[B](other: StagedStream[B]) =
    StagedStream[(A,B)](stream zip other.stream)

  def flatMap[B](f: StagedParameterizedStream[A, B]) =
    StagedStream[B](stream >>> f)
}
