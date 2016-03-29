package nz.rd.lang18

import scala.annotation.tailrec
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
    case AST.Assign(lhs, rhs) =>
      val value = bind(lhs, rhs, scope, BindMode.Assign)
      Value.Unt
    case AST.Func(name, args, body) =>
      val func = Value.Func(args, body, scope)
      scope.bindings += (name -> func)
      func
    case call: AST.Call =>
      val func = interpret0(call.lhs, scope).asInstanceOf[Value.Func]
      val evalScope = scope.createChild
      val argsValue = bind(func.args, call.args, evalScope, BindMode.Val)
      val callScope = Scope(evalScope.bindings, Some(scope)) // Copy new bindings into new call scope
      interpret0(func.body, callScope)
    case AST.Tup(values) =>
      Value.Tup(values.map(interpret0(_, scope)))
    case AST.Add(lhs, rhs) =>
      (interpret0(lhs, scope), interpret0(rhs, scope)) match {
        case (Value.Inr(a), Value.Inr(b)) => Value.Inr(a + b)
      }
    case AST.Symbol(name) =>
      val optValue = scope.lookup(name)
      assert(optValue.isDefined, s"Variable $name not declared")
      @tailrec
      def deref(v: Value): Value = v match {
        case Value.Var(Some(newValue)) => deref(newValue)
        case Value.Var(None) => sys.error(s"Var $name hasn't been set")
        case _ => v
      }
      deref(optValue.get)
    case AST.Inr(value) =>
      Value.Inr(value)
    case AST.Bool(value) =>
      Value.Bool(value)
    case AST.Str(value) =>
      Value.Str(value)
  }

  private def bind(bindAst: AST, valueAst: AST, scope: Scope, mode: BindMode): Value = bindAst match {
    case AST.Var(varAst) =>
      assert(mode != BindMode.Var, "var modifier is redundant")
      bind(varAst, valueAst, scope, BindMode.Var)
    case AST.Symbol(name) =>
      val value = interpret0(valueAst, scope)
      mode match {
        case BindMode.Var =>
          assert(!scope.bindings.contains(name))
          scope.bindings += (name -> Value.Var(Some(value))) // TODO: Use a mutable cell
        case BindMode.Val =>
          assert(!scope.bindings.contains(name))
          scope.bindings += (name -> value) // TODO: Use a mutable cell
        case BindMode.Assign =>
          assert(scope.bindings.contains(name))
          // Find the last var
          @tailrec
          def assignToVar(v: Value): Unit = v match {
            case Value.Var(Some(v1@Value.Var(_))) => assignToVar(v1)
            case v1@Value.Var(_) => v1.value = Some(value)
            case _ => sys.error(s"$name is not a var so it can't be assigned")
          }
          assignToVar(scope.bindings(name))
      }
      value
    case AST.Tup(bs) =>
      val vs = valueAst.asInstanceOf[AST.Tup].values
      Value.Tup((bs zip vs).map { case (b, v) => bind(b, v, scope, mode) })
  }

  sealed trait BindMode
  object BindMode {
    final case object Assign extends BindMode
    final case object Val extends BindMode
    final case object Var extends BindMode
  }

  sealed trait Value
  object Value {
    final case object Unt extends Value
    final case class Inr(value: Int) extends Value
    final case class Var(var value: Option[Value]) extends Value
    final case class Str(value: String) extends Value
    final case class Tup(values: immutable.Seq[Value]) extends Value
    final case class Bool(value: Boolean) extends Value
    final case class Func(args: AST.Tup, body: AST, lexScope: Scope) extends Value {
      override def toString: String = s"Func($args, $body, <scope>)"
    }
  }

  final case class Scope(bindings: mutable.Map[String,Value], parent: Option[Scope]) {
    def createChild: Scope = Scope(mutable.Map.empty, Some(this))
    def lookup(name: String): Option[Value] = {
      bindings.get(name) orElse { parent.flatMap(_.lookup(name)) }
    }
    override def toString: String = s"Scope($bindings,$parent)"
  }

}