package is.hail.cxx

import is.hail.nativecode.NativeStatus
import is.hail.utils._

object CompileUtils {

  def compileL1(emit: (FunctionBuilder, Code) => Code): Long => Long = {
    val tub = new TranslationUnitBuilder
    tub.include("<cstring>")
    tub.include("hail/hail.h")
    tub.include("hail/Utils.h")

    val f = {
      val fb = tub.buildFunction(tub.genSym("f"), Array("long" -> "arg"), "long")
      val body = emit(fb, fb.getArg(0).toString)
      println(body)
      fb += s"return ${body};"
      fb.end()
    }

    tub += new Definition {
      def name = "entrypoint"
      def define =
        s"""
           |long entrypoint(NativeStatus *st, long arg) {
           |  try {
           |    return ${f.name}(arg);
           |  } catch (const FatalError& e) {
           |    NATIVE_ERROR(st, 1005, e.what());
           |    return -1;
           |  }
           |}
         """.stripMargin
    }

    val tu = tub.end()
    val st = new NativeStatus()
    val entrypoint = {
      val mod = tu.build("-ggdb -O0")
      //mod.findOrBuild(st)
      //assert(st.ok, st.toString)
      val nf = mod.findLongFuncL1(st, "entrypoint")
      assert(st.ok, st.toString)
      mod.close()
      nf
    }

    arg => {
      val ret = entrypoint(st, arg)
      if (st.fail)
        fatal(st.toString())
      ret
    }
  }

}
