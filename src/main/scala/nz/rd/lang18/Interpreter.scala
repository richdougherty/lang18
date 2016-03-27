package nz.rd.lang18

object Interpreter {
  def interpret(ast: AST): Unit = ast match {
    case Print(expr) => evaluate(expr) match {
      case Value.Str(value) => println(value)
    }
  }

  private def evaluate(expr: Str): Value = expr match {
    case Str(value) => Value.Str(value)
  }

  private sealed trait Value
  private object Value {
    final case class Str(value: String) extends Value
  }
}