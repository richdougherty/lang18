package nz.rd.lang18

import org.parboiled2._

class Parser(val input: ParserInput) extends org.parboiled2.Parser {
  def print: Rule1[Print] = rule { "print" ~ " " ~ str ~> ((s: Str) => Print(s)) }
  def str: Rule1[Str] = {
    def str0: Rule1[Str] = rule { capture(zeroOrMore(CharPredicate.Alpha)) ~> ((s: String) => Str(s)) }
    rule {
       ("\"" ~ str0 ~ "\"") | ("'" ~ str0 ~ "'")
    }
  }
}