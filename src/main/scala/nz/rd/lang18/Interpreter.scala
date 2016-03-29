package nz.rd.lang18

import scala.collection.{immutable, mutable}

final object Interpreter {

  def interpret(ast: AST): Value = interpret0(ast, Scope(mutable.Map.empty, None))

  private def interpret0(ast: AST, scope: Scope): Value = ast match {
    case AST.Print(expr) =>
      interpret0(expr, scope) match {
        case Value.Unt => println("()")
        case Value.Inr(value) => println(value)
        case Value.Str(value) => println(value)
        case Value.Bool(value) => println(value)
      }
      Value.Unt
    case AST.Block(children: List[AST]) =>
      val blockScope = scope.createChild
      children.foldLeft[Value](Value.Unt) {
        case (_, child) => interpret0(child, blockScope)
      }
    case AST.Cond(c, t, f) =>
      interpret0(c, scope) match {
        case Value.Bool(true) => interpret0(t, scope)
        case Value.Bool(false) => interpret0(f, scope)
      }
    case AST.Var(name, rhs) =>
      val (value, bindings) = bind(AST.Symbol(name), rhs, scope)
      for (b <- bindings.keys) {
        assert(!scope.bindings.contains(b), s"$b already bound")
      }
      scope.bindings ++= bindings
      Value.Unt
    case AST.Func(name, args, body) =>
      val func = Value.Func(args, body, scope)
      scope.bindings += (name -> func)
      func
    case call: AST.Call =>
      val func = interpret0(call.lhs, scope).asInstanceOf[Value.Func]
      val (argsValue, argsBindings) = bind(func.args, call.args, scope)
      val callScope = func.lexScope.createChild
      callScope.bindings ++= argsBindings
      interpret0(func.body, callScope)
    case AST.Tup(values) =>
      Value.Tup(values.map(interpret0(_, scope)))
    case AST.Add(lhs, rhs) =>
      (interpret0(lhs, scope), interpret0(rhs, scope)) match {
        case (Value.Inr(a), Value.Inr(b)) => Value.Inr(a + b)
      }
    case AST.Symbol(name) =>
      assert(scope.bindings.contains(name), s"Variable $name not declared")
      scope.bindings(name)
    case AST.Inr(value) =>
      Value.Inr(value)
    case AST.Bool(value) =>
      Value.Bool(value)
    case AST.Str(value) =>
      Value.Str(value)
  }

  private def bind(bindAst: AST, valueAst: AST, scope: Scope): (Value, mutable.Map[String,Value]) = {
    val bindings = mutable.Map.empty[String,Value]
    val value = bind0(bindAst, valueAst, scope.createChild, bindings)
    (value, bindings)
  }

  private def bind0(bindAst: AST, valueAst: AST, evalScope: Scope, bindings: mutable.Map[String,Value]): Value = (bindAst, valueAst) match {
    case (AST.Symbol(name), valueAst) =>
      val value = interpret0(valueAst, evalScope)
      bindings += (name -> value)
      value
    case (AST.Tup(bs), AST.Tup(vs)) =>
      Value.Tup((bs zip vs).map { case (b, v) => bind0(b, v, evalScope, bindings) })
  }

  sealed trait Value
  object Value {
    final case object Unt extends Value
    final case class Inr(value: Int) extends Value
    final case class Str(value: String) extends Value
    final case class Tup(values: immutable.Seq[Value]) extends Value
    final case class Bool(value: Boolean) extends Value
    final case class Func(args: AST.Tup, body: AST, lexScope: Scope) extends Value {
      override def toString: String = s"Func($args, $body, <scope>)"
    }
  }

  final case class Scope(bindings: mutable.Map[String,Value], parent: Option[Scope]) {
    def createChild: Scope = Scope(mutable.Map.empty, Some(this))
  }

}