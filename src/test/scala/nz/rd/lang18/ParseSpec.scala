package nz.rd.lang18

import collection.mutable.Stack
import org.scalatest._
import scala.util.{Failure, Success, Try}

import AST._

class ParseSpec extends FreeSpec with Matchers {

  def tryParse(program: String): Try[AST] = {
    val parser = new Parser(program)
    parser.program.run()
  }

  "Parsing" - {
    "should handle hello world" in {
      assert(tryParse("print 'Hello'") == Success(Block(List(Print(Str("Hello"))))))
    }
    "should handle symbols" in {
      assert(tryParse("x") == Success(Block(List(Symbol("x")))))
    }
    "should handle var declarations" in {
      assert(tryParse("var x") == Success(Block(List(Var(Symbol("x"))))))
    }
    "should handle var declarations and assignments" in {
      assert(tryParse("var x = 1") == Success(Block(List(Assign(Var(Symbol("x")), Inr(1))))))
    }
    "should handle empty lines" in {
      assert(tryParse("print 1\n\nprint 2\n\n") == Success(Block(List(Print(Inr(1)), Print(Inr(2))))))
    }
    "should handle boolean values as symbols" in {
      assert(tryParse("true") == Success(Block(List(Symbol("true")))))
    }
    "should handle numbers" in {
      assert(tryParse("1") == Success(Block(List(Inr(1)))))
    }
    "should handle addition" in {
      assert(tryParse("1 + 2") == Success(Block(List(Bin(Inr(1), Bin.Op.Add, Inr(2))))))
    }
  }

}
