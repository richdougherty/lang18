package nz.rd.lang18

import collection.mutable.Stack
import org.scalatest._

class ParseSpec extends FreeSpec with Matchers {

  "Parsing" - {
    "should handle hello world" in {
      val parser = new Parser("print 'Hello'")
      val ast = parser.print.run().get
      assert(ast === Print(Str("Hello")))
    }
    "should handle multiple lines" in {
      val parser = new Parser("print 'Hello'\nprint 'Goodbye'")
      val ast = parser.block.run().get
      assert(ast === Block(List(Print(Str("Hello")), Print(Str("Goodbye")))))
    }
  }

}
