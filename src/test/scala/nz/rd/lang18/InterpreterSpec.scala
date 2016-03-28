package nz.rd.lang18

import collection.mutable.Stack
import org.scalatest._
import scala.util.{Success, Try}

class InterpreterSpec extends FreeSpec with Matchers {

  def interpret(program: String): Try[Interpreter.Value] = {
    val parser = new Parser(program)
    val parse: Try[AST] = parser.program.run()
    parse.map(Interpreter.interpret(_))
  }

  "Interpreting" - {
    "should handle hello world" in {
      assert(interpret("print 'Hello'") === Success(Interpreter.Value.Unt))
    }
    "should handle multiple lines" in {
      assert(interpret("print 'Hello'\nprint 'Goodbye'") === Success(Interpreter.Value.Unt))
    }
    "should handle conditions" in {
      assert(interpret("if true { 'true' } else { 'false' }") === Success(Interpreter.Value.Str("true")))
    }
    "should handle variables" in {
      assert(interpret("var i = 1\ni") === Success(Interpreter.Value.Inr(1)))
    }
  }

}
