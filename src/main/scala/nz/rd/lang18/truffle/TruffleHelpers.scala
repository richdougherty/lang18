package nz.rd.lang18.truffle

import com.oracle.truffle.api.TruffleLanguage
import com.oracle.truffle.api.frame.FrameDescriptor
import com.oracle.truffle.api.source.Source
import nz.rd.lang18.truffle.nodes._
import nz.rd.lang18.{AST, Parser}
import org.graalvm.polyglot.Context
import org.parboiled2.ParserInput

object TruffleHelpers {
  def parse(lang: Lang18Language, source: Source): L18RootNode = {
    val parserInput: ParserInput = source.getCharacters.toString
    val ast: AST  = new Parser(parserInput).program.run().get
    val node: L18ExpressionNode = convertAST(ast)
    new L18RootNode(lang, new FrameDescriptor, node)
  }


  def convertAST(ast: AST): L18ExpressionNode = ast match {
    case AST.Print(expr) => new L18PrintNode(convertAST(expr))
    //case AST.Parens(child) => new L18ParensNode(convertAST(child))
    case AST.Block(children: List[AST]) =>
      //val blockScope = scope.createChild
      new L18BlockNode(children.toArray.map(convertAST))
//
//      children.foldLeft[Value](Value.Unt) {
//        case (_, child) => interpret0(child, blockScope)
//      }
//    case AST.Cond(c, t, f) =>
//      interpret0(c, scope) match {
//        case Value.Bool(true) => interpret0(t, scope)
//        case Value.Bool(false) => interpret0(f, scope)
//      }
//    case AST.Assign(lhs, rhs) =>
//      val value = bind(lhs, rhs, scope, BindMode.Assign)
//      Value.Unt
//    case AST.Func(name, args, body) =>
//      val func = Value.Func(args, body, scope)
//      scope.bindings += (name -> func)
//      func
//    case call: AST.Call =>
//      val func = interpret0(call.lhs, scope).asInstanceOf[Value.Func]
//      val evalScope = scope.createChild
//      val argsValue = bind(func.args, call.args, evalScope, BindMode.Val)
//      val callScope = Scope(evalScope.bindings, Some(scope)) // Copy new bindings into new call scope
//      interpret0(func.body, callScope)
//    case AST.Cons(head, tail) =>
//      Value.Cons(interpret0(head, scope), interpret0(tail, scope))
//    case AST.Bin(lhs, op, rhs) =>
//      (interpret0(lhs, scope), op, interpret0(rhs, scope)) match {
//        case (Value.Inr(a), AST.Bin.Op.Add, Value.Inr(b)) => Value.Inr(a + b)
//        case (Value.Inr(a), AST.Bin.Op.Sub, Value.Inr(b)) => Value.Inr(a - b)
//        case (Value.Inr(a), AST.Bin.Op.Mul, Value.Inr(b)) => Value.Inr(a * b)
//        case (Value.Inr(a), AST.Bin.Op.Equals, Value.Inr(b)) => Value.Bool(a == b)
//        case (Value.Inr(a), AST.Bin.Op.LessThan, Value.Inr(b)) => Value.Bool(a < b)
//      }
//    case AST.Symbol(name) =>
//      val optValue = scope.lookup(name)
//      assert(optValue.isDefined, s"Variable $name not declared")
//      @tailrec
//      def deref(v: Value): Value = v match {
//        case Value.Var(Some(newValue)) => deref(newValue)
//        case Value.Var(None) => sys.error(s"Var $name hasn't been set")
//        case _ => v
//      }
//      deref(optValue.get)
//    case AST.Inr(value) =>
//      Value.Inr(value)
    case AST.Str(value) => new L18StringNode(value)
  }
}
