package nz.rd.lang18

import collection.mutable.Stack
import org.scalatest._
import scala.util.Success

class InterpreterSpec extends FreeSpec with Matchers {

  "Interpreting" - {
    "should handle hello world" in {
      val parser = new Parser("print 'Hello'")
      val ast = parser.print.run().get
      assert(Interpreter.interpret(ast) === ())
    }
    "should handle multiple lines" in {
      val parser = new Parser("print 'Hello'\nprint 'Goodbye'")
      val ast = parser.program.run().get
      assert(Interpreter.interpret(ast) === ())
    }
    "should handle conditions" in {
      val parser = new Parser("if true { print 'true' } else { print 'false' }")
      val ast = parser.program.run()

      assert(ast.map(Interpreter.interpret(_)) === Success(()))
    }
  }

}
