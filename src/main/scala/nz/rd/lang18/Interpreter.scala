package nz.rd.lang18

import scala.collection.{immutable, mutable}

final object Interpreter {

  def interpret(ast: AST): Value = interpret0(ast, Scope(mutable.Map.empty, None))

  private def interpret0(ast: AST, scope: Scope): Value = ast match {
    case Print(expr) =>
      interpret0(expr, scope) match {
        case Value.Unt => println("()")
        case Value.Inr(value) => println(value)
        case Value.Str(value) => println(value)
        case Value.Bool(value) => println(value)
      }
      Value.Unt
    case Block(children: List[AST]) =>
      val blockScope = scope.createChild
      children.foldLeft[Value](Value.Unt) {
        case (_, child) => interpret0(child, blockScope)
      }
    case Cond(c, t, f) =>
      interpret0(c, scope) match {
        case Value.Bool(true) => interpret0(t, scope)
        case Value.Bool(false) => interpret0(f, scope)
      }
    case Var(name, rhs) =>
      assert(!scope.bindings.contains(name), s"Variable $name already bound")
      val value = interpret0(rhs, scope)
      scope.bindings += (name -> value)
      Value.Unt
    case Func(name, args, body) =>
      val func = Value.Func(args, body, scope)
      scope.bindings += (name -> func)
      func
    case Call(lhs, args) =>
      val func = interpret0(lhs, scope).asInstanceOf[Value.Func]
      val argsValue = interpret0(args, scope).asInstanceOf[Value.Tup]
      assert(argsValue.values.length == func.args.values.length)

      val callScope = func.lexScope.createChild
      for ((arg, argValue) <- func.args.values.zip(argsValue.values)) {
        val argSymbol = arg.asInstanceOf[Symbol]
        callScope.bindings += (argSymbol.name -> argValue)
      }
      interpret0(func.body, callScope)
    case Tup(values) =>
      Value.Tup(values.map(interpret0(_, scope)))
    case Add(lhs, rhs) =>
      (interpret0(lhs, scope), interpret0(rhs, scope)) match {
        case (Value.Inr(a), Value.Inr(b)) => Value.Inr(a + b)
      }
    case Symbol(name) =>
      assert(scope.bindings.contains(name), s"Variable $name not declared")
      scope.bindings(name)
    case Inr(value) =>
      Value.Inr(value)
    case Bool(value) =>
      Value.Bool(value)
    case Str(value) =>
      Value.Str(value)
  }

  // private def bind0(ast: AST, ast: AST, scope: Scope): Unit = ast match {
  //   case Symbol(name) =>
  //     assert(!scope.bindings.contains(name), s"Variable $name already bound")
  //     val value = interpret0(rhs, scope)
  //     scope.bindings += (name -> value)
  //     Value.Unt
  // }

  sealed trait Value
  object Value {
    final case object Unt extends Value
    final case class Inr(value: Int) extends Value
    final case class Str(value: String) extends Value
    final case class Tup(values: immutable.Seq[Value]) extends Value
    final case class Bool(value: Boolean) extends Value
    final case class Func(args: nz.rd.lang18.Tup, body: AST, lexScope: Scope) extends Value {
      override def toString: String = s"Func($args, $body, <scope>)"
    }
  }

  final case class Scope(bindings: mutable.Map[String,Value], parent: Option[Scope]) {
    def createChild: Scope = Scope(mutable.Map.empty, Some(this))
  }

}