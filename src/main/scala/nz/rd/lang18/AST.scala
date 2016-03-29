package nz.rd.lang18

import scala.collection.immutable

sealed trait AST
final object AST {
  final case class Block(children: immutable.Seq[AST]) extends AST
  final case class Inr(value: Int) extends AST
  final case class Bool(value: Boolean) extends AST
  final case class Str(value: String) extends AST
  final case class Tup(values: immutable.Seq[AST]) extends AST
  final case class Print(arg: AST) extends AST
  final case class Cond(test: AST, trueBranch: AST, falseBranch: AST) extends AST
  final case class Var(ast: AST) extends AST
  final case class Symbol(name: String) extends AST
  final case class Func(name: String, args: Tup, body: AST) extends AST
  final case class Call(lhs: AST, args: Tup) extends AST
  final case class Assign(lhs: AST, rhs: AST) extends AST
  final case class Add(lhs: AST, rhs: AST) extends AST
}