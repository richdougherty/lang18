package nz.rd.lang18

import scala.collection.immutable

sealed trait AST
final object AST {
  final case class Block(children: immutable.Seq[AST]) extends AST
  final case class Parens(child: AST) extends AST
  final case class Inr(value: Int) extends AST
  final case class Str(value: String) extends AST
  final case class Cons(head: AST, tail: AST) extends AST
  final case object Unit extends AST
  final case class Print(arg: AST) extends AST
  final case class Cond(test: AST, trueBranch: AST, falseBranch: AST) extends AST
  final case class Var(ast: AST) extends AST
  final case class Symbol(name: String) extends AST
  final case class Func(name: String, args: AST, body: AST) extends AST
  final case class Call(lhs: AST, args: AST) extends AST
  final case class Assign(lhs: AST, rhs: AST) extends AST
  final case class Bin(lhs: AST, op: Bin.Op, rhs: AST) extends AST
  object Bin {
    sealed trait Op
    object Op {
      // comparison
      final case object Equals extends Op
      final case object LessThan extends Op
      // add
      final case object Add extends Op
      final case object Sub extends Op
      // mul
      final case object Mul extends Op
    }
  }

  final case class Ann(lhs: AST, rhs: AST) extends AST
}