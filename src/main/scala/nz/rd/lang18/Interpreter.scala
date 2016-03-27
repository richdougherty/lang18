package nz.rd.lang18

object Interpreter {
  def interpret(ast: AST): Unit = ast match {
    case Print(expr) => evaluate(expr) match {
      case Value.Inr(value) => println(value)
      case Value.Str(value) => println(value)
    }
  }

  private def evaluate(expr: AST): Value = expr match {
    case Inr(value) => Value.Inr(value)
    case Str(value) => Value.Str(value)
  }

  private sealed trait Value
  private object Value {
    final case class Inr(value: Int) extends Value
    final case class Str(value: String) extends Value
  }
}