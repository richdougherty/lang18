package nz.rd.lang18

import collection.mutable.Stack
import org.scalatest._
import scala.util.Success

import AST._

class ParseSpec extends FreeSpec with Matchers {

  "Parsing" - {
    "should handle hello world" in {
      val parser = new Parser("print 'Hello'")
      val ast = parser.program.run().get
      assert(ast === Block(List(Print(Str("Hello")))))
    }
    "should handle symbols" in {
      val parser = new Parser("x")
      val parse = parser.program.run()
      assert(parse === Success(Block(List(Symbol("x")))))
    }
    "should handle var declarations" in {
      val parser = new Parser("var x")
      val parse = parser.program.run()
      assert(parse === Success(Block(List(Var(Symbol("x"))))))
    }
    "should handle var declarations and assignments" in {
      val parser = new Parser("var x = 1")
      val parse = parser.program.run()
      assert(parse === Success(Block(List(Assign(Var(Symbol("x")), Inr(1))))))
    }
    "should handle empty lines" in {
      val parser = new Parser("print 1\n\nprint 2\n\n")
      val parse = parser.program.run()
      assert(parse === Success(Block(List(Print(Inr(1)), Print(Inr(2))))))
    }
    "should handle boolean values as symbols" in {
      val parser = new Parser("true")
      val ast = parser.program.run().get
      assert(ast === Block(List(Symbol("true"))))
    }
    "should handle numbers" in {
      val parser = new Parser("1")
      val ast = parser.program.run().get
      assert(ast === Block(List(Inr(1))))
    }
    "should handle addition" in {
      val parser = new Parser("1 + 2")
      val ast = parser.program.run().get
      assert(ast === Block(List(Add(Inr(1), Inr(2)))))
    }
  }

}
