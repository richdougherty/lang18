package nz.rd.lang18

import org.parboiled2._
import scala.collection.immutable

import AST._

class Parser(val input: ParserInput) extends org.parboiled2.Parser {

  // Entry point for a program

  def program: Rule1[AST] = rule { lines ~ EOI }

  def ast: Rule1[AST] = astIntrinsic

  // INTRINSIC //

  def astIntrinsic: Rule1[AST] = rule { cond | print | func /* FIXME */ | astAssign }

  def print: Rule1[Print] = rule { "print " ~ ast ~> ((a: AST) => Print(a)) }

  def cond: Rule1[Cond] = rule {
    "if " ~ ast ~ " " ~ block ~ " else " ~ block ~> Cond
  }

  def func: Rule1[Func] = rule {
    "def " ~ ident ~ args ~ " " ~ block ~> Func
  }

  // ASSIGN //

  def astAssign: Rule1[AST] = rule { assign | astComma }

  def assign: Rule1[Assign] = rule {
    astComma ~ " = " ~ ast ~> Assign
  }

  // LIST //

  def astComma: Rule1[AST] = rule { cons | astModifier }

  def cons: Rule1[Cons] = rule {
    astModifier ~ ", " ~ ast ~> Cons
  }

  // MODIFIER //

  def astModifier: Rule1[AST] = rule { `var` | astAnnotation }

  def `var`: Rule1[Var] = rule {
    "var " ~ astAnnotation ~> Var
  }

  // ANNOTATION //

  def astAnnotation: Rule1[AST] = rule { annotation | astLogic }

  def annotation: Rule1[Ann] = rule {
    astLogic ~ ": " ~ astLogic ~> Ann
  }

  // LOGIC //

  def astLogic: Rule1[AST] = astCompare

  // COMPARE //

  def astCompare: Rule1[AST] = rule { equals | astShift }

  def equals: Rule1[Bin] = rule {
    astShift ~ " " ~ cmpOp ~ " " ~ ast ~> ((lhs: AST, op: Bin.Op, rhs: AST) => Bin(lhs, op, rhs))
  }

  def cmpOp: Rule1[Bin.Op] = rule {
    ("==" ~ push(Bin.Op.Equals)) |
    ("<" ~ push(Bin.Op.LessThan))
  }

  // SHIFT //

  def astShift: Rule1[AST] = astAdd

  // ADD //

  def astAdd: Rule1[AST] = rule { add | astMult }

  def add: Rule1[Bin] = rule {
    astMult ~ " " ~ addOp ~ " " ~ ast ~> ((lhs: AST, op: Bin.Op, rhs: AST) => Bin(lhs, op, rhs))
  }

  def addOp: Rule1[Bin.Op] = rule {
    ("+" ~ push(Bin.Op.Add)) |
    ("-" ~ push(Bin.Op.Sub))
  }

  // MULT //

  def astMult: Rule1[AST] = rule { mult | astUnary }

  def mult: Rule1[Bin] = rule {
    astUnary ~ " " ~ multOp ~ " " ~ ast ~> ((lhs: AST, op: Bin.Op, rhs: AST) => Bin(lhs, op, rhs))
  }

  def multOp: Rule1[Bin.Op] = rule {
    ("*" ~ push(Bin.Op.Mul))
  }



  // UNARY //

  def astUnary: Rule1[AST] = astApplyAccess
 
  // APPLY/ACCESS //

  def astApplyAccess: Rule1[AST] = rule { call | args | astCombine }

  def call: Rule1[Call] = rule {
    astCombine ~ astCombine ~> Call
  }

  def args: Rule1[AST] = rule {
    astCombine | (" " ~ astCombine)
  }

  // COMBINE //

  def astCombine: Rule1[AST] = rule { parens | block | astPrimitive }

  def parens: Rule1[AST] = rule {
    "(" ~ ast ~ ")" ~> Parens
  }

  def block: Rule1[AST] = rule {
    "{" ~
    (
      (" " ~ ast ~ " ") | ("\n" ~ lines ~ "\n")
    ) ~
    "}"
  }

  def lines: Rule1[Block] = rule {
    zeroOrMore(optional(ast)).separatedBy("\n") ~> ((s: immutable.Seq[Option[AST]]) => Block(s.flatten.toList))
  }

  // PRIMITIVE //

  def astPrimitive: Rule1[AST] = rule {
    inr | str | symbol
  }


  def inr: Rule1[Inr] = rule {
    capture(oneOrMore(CharPredicate.Digit)) ~> ((digits: String) => Inr(Integer.parseInt(digits)))
  }
  // def tup: Rule1[Tup] = rule {
  //   "(" ~ zeroOrMore(ast).separatedBy(", ") ~ ")" ~> Tup
  // }

  def str: Rule1[Str] = {
    def str0: Rule1[String] = rule { capture(zeroOrMore(CharPredicate.Alpha)) }
    rule {
     (("\"" ~ str0 ~ "\"") | ("'" ~ str0 ~ "'")) ~> Str
    }
  }

  def symbol: Rule1[Symbol] = rule {
    ident ~> Symbol
  }

  // Non-AST rules

  def ident: Rule1[String] = rule { capture(oneOrMore(CharPredicate.Alpha)) }
}