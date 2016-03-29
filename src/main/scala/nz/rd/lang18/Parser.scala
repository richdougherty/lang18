package nz.rd.lang18

import org.parboiled2._
import scala.collection.immutable

class Parser(val input: ParserInput) extends org.parboiled2.Parser {
  def program: Rule1[AST] = rule { multi ~ EOI }
  def multi: Rule1[Block] = rule {
    zeroOrMore(optional(ast)).separatedBy("\n") ~> ((s: immutable.Seq[Option[AST]]) => Block(s.flatten.toList))
  }
  def block: Rule1[AST] = rule {
    "{" ~
    (
      (" " ~ ast ~ " ") | ("\n" ~ multi ~ "\n")
    ) ~
    "}"
  }
  def print: Rule1[Print] = rule { "print " ~ ast ~> ((a: AST) => Print(a)) }
  def cond: Rule1[Cond] = rule {
    "if " ~ ast ~ " " ~ block ~ " else " ~ block ~> Cond
  }
  def ast: Rule1[AST] = rule {
    str | inr | bool |
    block | `var` | cond |
    print |
    symbol
  }
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
  def `var`: Rule1[Var] = rule {
    "var " ~ capture(oneOrMore(CharPredicate.Alpha)) ~ " = " ~ ast ~> Var
  }
  def symbol: Rule1[Symbol] = rule {
    capture(oneOrMore(CharPredicate.Alpha)) ~> Symbol
  }
}