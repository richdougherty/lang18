package nz.rd.lang18

import java.nio.file.{Files, Paths}

import com.oracle.truffle.api.{CallTarget, Truffle}
import nz.rd.lang18.truffle.{Lang18Language, TruffleHelpers}
import nz.rd.lang18.truffle.nodes.L18RootNode
import org.graalvm.polyglot.{Context, Source}

object Main {
  def main(args: Array[String]): Unit = {
    val fileName = args(0)
    val sourcePath = Paths.get(fileName)

     //val source = new String(Files.readAllBytes(sourcePath))
    // val ast = (new Parser(source)).program.run().get
    // Interpreter.interpret(ast)

    val ctx = Context.newBuilder(Lang18Language.ID).in(System.in).build()
    val source = Source.newBuilder(Lang18Language.ID, sourcePath.toFile).build
    ctx.eval(source)

//    val lang = new Lang18Language()
//    val ctx = Context.newBuilder(Lang18Language.ID).in(System.in).build()
//    val rootNode = TruffleHelpers.parse(lang, source)
//    val callTarget = Truffle.getRuntime.createCallTarget(rootNode)
//    callTarget.call()
  }
}