package nz.rd.lang18

import org.parboiled2._
import scala.collection.immutable

class Parser(val input: ParserInput) extends org.parboiled2.Parser {

  // Entry point for a program

  def program: Rule1[AST] = rule { multi ~ EOI }

  // Multi-line ASTs

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
  // AST

  def ast: Rule1[AST] = astUnit

  //// astUnit - ASTs that evaluate to unit so won't have any suffix ////

  def astUnit: Rule1[AST] = rule {
    `var` | print | astSuffix
  }

  def `var`: Rule1[Var] = rule {
    "var " ~ ident ~ " = " ~ ast ~> Var
  }
  def print: Rule1[Print] = rule { "print " ~ ast ~> ((a: AST) => Print(a)) }

  //// astSuffix - ASTs that begin with another AST ////

  def astSuffix: Rule1[AST] = {
    rule {
      add | call | astSimple
    }
  }

  def add: Rule1[Add] = rule {
    astSimple ~ " + " ~ ast ~> Add
  }
  def call: Rule1[Call] = rule {
    astSimple ~ "(" ~ zeroOrMore(ast).separatedBy(", ") ~ ")" ~> Call
  }

  //// astSimple - ASTs that start with an unambiguous prefix ////

  def astSimple: Rule1[AST] = {

    rule {
      // Literals
      str | inr | bool |
      // Reserved words
      block | func | cond |
      // Symbol
      symbol
    }
  }

  def bool: Rule1[Bool] = rule { "true" ~ push(Bool(true)) | "false" ~ push(Bool(false)) }
  def inr: Rule1[Inr] = rule {
    capture(oneOrMore(CharPredicate.Digit)) ~> ((digits: String) => Inr(Integer.parseInt(digits)))
  }
  // str rule doesn't compile in this scope for some reason
  def symbol: Rule1[Symbol] = rule {
    ident ~> Symbol
  }

  def str: Rule1[Str] = {
    def str0: Rule1[String] = rule { capture(zeroOrMore(CharPredicate.Alpha)) }
    rule {
     (("\"" ~ str0 ~ "\"") | ("'" ~ str0 ~ "'")) ~> Str
    }
  }

  def cond: Rule1[Cond] = rule {
    "if " ~ ast ~ " " ~ block ~ " else " ~ block ~> Cond
  }
  def func: Rule1[Func] = rule {
    "def " ~ ident ~ "(" ~ zeroOrMore(ident).separatedBy(", ") ~ ") " ~ block ~> Func
  }

  // Non-AST rules

  def ident: Rule1[String] = rule { capture(oneOrMore(CharPredicate.Alpha)) }
}