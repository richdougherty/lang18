package nz.rd.lang18

final object Interpreter {
  def interpret(ast: AST): Unit = ast match {
    case Print(expr) => evaluate(expr) match {
      case Value.Inr(value) => println(value)
      case Value.Str(value) => println(value)
    }
    case Block(children: List[AST]) => children.foreach(interpret(_))
    case Cond(c, t, f) => evaluate(c) match {
      case Value.Bool(true) => interpret(t)
      case Value.Bool(false) => interpret(f)
    }
  }

  private def evaluate(expr: AST): Value = expr match {
    case Inr(value) => Value.Inr(value)
    case Bool(value) => Value.Bool(value)
    case Str(value) => Value.Str(value)
  }

  private sealed trait Value
  private object Value {
    final case class Inr(value: Int) extends Value
    final case class Str(value: String) extends Value
    final case class Bool(value: Boolean) extends Value
  }
}