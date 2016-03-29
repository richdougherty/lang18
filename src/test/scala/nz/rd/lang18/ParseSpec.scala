package nz.rd.lang18

import collection.mutable.Stack
import org.scalatest._
import scala.util.Success

class ParseSpec extends FreeSpec with Matchers {

  "Parsing" - {
    "should handle hello world" in {
      val parser = new Parser("print 'Hello'")
      val ast = parser.print.run().get
      assert(ast === Print(Str("Hello")))
    }
    "should handle var declarations" in {
      val parser = new Parser("var x = 1")
      val parse = parser.`var`.run()
      assert(parse === Success(Var("x", Inr(1))))
    }
    "should handle empty lines" in {
      val parser = new Parser("print 1\n\nprint 2\n\n")
      val parse = parser.program.run()
      assert(parse === Success(Block(List(Print(Inr(1)), Print(Inr(2))))))
    }
    "should handle symbols" in {
      val parser = new Parser("x")
      val parse = parser.symbol.run()
      assert(parse === Success(Symbol("x")))
    }
    "should handle boolean values" in {
      val parser = new Parser("true")
      val ast = parser.program.run().get
      assert(ast === Block(List(Bool(true))))
    }
  }

}
