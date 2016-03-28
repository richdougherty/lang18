package nz.rd.lang18

final object Interpreter {
  def interpret(ast: AST): Value = ast match {
    case Print(expr) =>
      interpret(expr) match {
        case Value.Unt => println("()")
        case Value.Inr(value) => println(value)
        case Value.Str(value) => println(value)
        case Value.Bool(value) => println(value)
      }
      Value.Unt
    case Block(children: List[AST]) =>
      children.foldLeft[Value](Value.Unt) {
        case (_, child) => interpret(child)
      }
    case Cond(c, t, f) =>
      interpret(c) match {
        case Value.Bool(true) => interpret(t)
        case Value.Bool(false) => interpret(f)
      }
    case Inr(value) =>
      Value.Inr(value)
    case Bool(value) =>
      Value.Bool(value)
    case Str(value) =>
      Value.Str(value)
  }

  sealed trait Value
  object Value {
    final case object Unt extends Value
    final case class Inr(value: Int) extends Value
    final case class Str(value: String) extends Value
    final case class Bool(value: Boolean) extends Value
  }
}