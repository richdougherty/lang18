package nz.rd.lang18

import collection.mutable.Stack
import org.scalatest._

class InterpreterSpec extends FreeSpec with Matchers {

  "Parsing" - {
    "should handle hello world" in {
      val parser = new Parser("print 'Hello'")
      val ast = parser.print.run().get
      assert(Interpreter.interpret(ast) === ())
    }
    "should handle multiple lines" in {
      val parser = new Parser("print 'Hello'\nprint 'Goodbye'")
      val ast = parser.block.run().get
      assert(Interpreter.interpret(ast) === ())
    }
  }

}
