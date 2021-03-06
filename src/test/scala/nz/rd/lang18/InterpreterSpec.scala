package nz.rd.lang18

import collection.mutable.Stack
import org.scalatest._
import scala.util.{Failure, Success, Try}

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
    "should handle variable assignment" in {
      assert(interpret("var i = 1\ni = i + 1\ni") === Success(Interpreter.Value.Inr(2)))
    }
    "should handle functions" in {
      assert(interpret("def incr(x) { x + 1 }\nincr(0)") === Success(Interpreter.Value.Inr(1)))
    }
    "should handle assignment to typed var" in {
      assert(interpret("var x: int = 1\nx") === Success(Interpreter.Value.Inr(1)))
    }
    "should handle incorrect assignment to typed var" in {
      interpret("var x: bool = 1\nx") shouldBe a[Failure[_]]
    }
    "should handle ints, bools, addition, equality, conditions and strings" in {
      assert(interpret(
        """|var x = 1
           |var y = 2
           |var b = (x + 1) == y
           |if (b) { "equal" } else { "unequal" }""".stripMargin
     ) === Success(Interpreter.Value.Str("equal")))
    }
    "should handle factorial" in {
      assert(interpret(
        """|def factorial(n) { if (n < 1) { 1 } else { factorial(n - 1) * n } }
           |factorial(4)""".stripMargin
     ) === Success(Interpreter.Value.Inr(24)))
    }
  }

}
