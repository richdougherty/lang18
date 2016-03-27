package nz.rd.lang18

sealed trait AST
final case class Inr(value: Int) extends AST
final case class Str(value: String) extends AST
final case class Print(arg: AST) extends AST
