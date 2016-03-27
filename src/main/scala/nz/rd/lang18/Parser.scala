package nz.rd.lang18

import org.parboiled2._

class Parser(val input: ParserInput) extends org.parboiled2.Parser {
  def print: Rule1[Print] = rule { "print" ~ " " ~ str ~> ((s: Str) => Print(s)) }
  def expr: Rule1[AST] = rule { str | inr }
  def str: Rule1[Str] = {
    def str0: Rule1[Str] = rule { capture(zeroOrMore(CharPredicate.Alpha)) ~> Str }
    rule {
       ("\"" ~ str0 ~ "\"") | ("'" ~ str0 ~ "'")
    }
  }
  def inr: Rule1[Inr] = rule {
    capture(oneOrMore(CharPredicate.Digit)) ~> ((digits: String) => Inr(Integer.parseInt(digits)))
  }
}