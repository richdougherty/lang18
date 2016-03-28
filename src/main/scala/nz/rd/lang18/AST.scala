package nz.rd.lang18

sealed trait AST
final case class Block(children: List[AST]) extends AST
final case class Inr(value: Int) extends AST
final case class Bool(value: Boolean) extends AST
final case class Str(value: String) extends AST
final case class Print(arg: AST) extends AST
final case class Cond(test: AST, trueBranch: AST, falseBranch: AST) extends AST
final case class Var(name: String, value: AST) extends AST
final case class Symbol(name: String) extends AST