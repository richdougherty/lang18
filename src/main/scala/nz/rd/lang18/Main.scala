package nz.rd.lang18

import java.nio.file.Files
import java.nio.file.Paths

object Main {
  def main(args: Array[String]): Unit = {
    val fileName = args(0)
    val source = new String(Files.readAllBytes(Paths.get(fileName)))
    val ast = (new Parser(source)).program.run().get
    Interpreter.interpret(ast)
  }
}