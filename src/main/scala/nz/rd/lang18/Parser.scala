package nz.rd.lang18

import org.parboiled2._

class Parser(val input: ParserInput) extends org.parboiled2.Parser {
  def program: Rule1[AST] = rule { multi ~ EOI }
  def multi: Rule1[Block] = rule {
    zeroOrMore(ast).separatedBy("\n") ~> ((s: Seq[AST]) => Block(s.toList))
  }
  def block: Rule1[AST] = rule {
    "{" ~
    (
      (" " ~ ast ~ " ") | ("\n" ~ multi ~ "\n")
    ) ~
    "}"
  }
  def print: Rule1[Print] = rule { "print " ~ str ~> ((s: Str) => Print(s)) }
  def cond: Rule1[Cond] = rule {
    "if " ~ ast ~ " " ~ block ~ " else " ~ block ~> Cond
  }
  def ast: Rule1[AST] = rule { str | inr | bool | print | cond }
  def bool: Rule1[Bool] = rule { "true" ~ push(Bool(true)) | "false" ~ push(Bool(false)) }
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