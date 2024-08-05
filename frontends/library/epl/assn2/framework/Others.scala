package epl.assn2.framework

import stainless.lang.*
import stainless.annotation.*

object Others {
  @library
  type Variable = BigInt
  @library
  type Env[A] = Map[Variable, A]

  // Arithmetic expressions

  @library
  sealed abstract class Expr

  @library
  case class Num(n: BigInt) extends Expr

  @library
  case class Plus(e1: Expr, e2: Expr) extends Expr

  @library
  case class Minus(e1: Expr, e2: Expr) extends Expr

  @library
  case class Times(e1: Expr, e2: Expr) extends Expr

  // Booleans
  @library
  case class Bool(n: Boolean) extends Expr

  @library
  case class Eq(e1: Expr, e2: Expr) extends Expr

  @library
  case class IfThenElse(e: Expr, e1: Expr, e2: Expr) extends Expr

  // Strings
  @library
  case class Str(s: BigInt) extends Expr

  @library
  case class Length(e: Expr) extends Expr

  @library
  case class Index(e1: Expr, e2: Expr) extends Expr

  @library
  case class Concat(e1: Expr, e2: Expr) extends Expr

  // Variables and let-binding
  @library
  case class Var(x: Variable) extends Expr

  @library
  case class Let(x: Variable, e1: Expr, e2: Expr) extends Expr

  @library
  case class LetFun(f: Variable, arg: Variable, ty: Type, e1: Expr, e2: Expr)
    extends Expr

  @library
  case class LetRec(f: Variable, arg: Variable, xty: Type, ty: Type, e1: Expr, e2: Expr)
    extends Expr

  @library
  case class LetPair(x: Variable, y: Variable, e1: Expr, e2: Expr) extends Expr

  // Pairing
  @library
  case class Pair(e1: Expr, e2: Expr) extends Expr

  @library
  case class First(e: Expr) extends Expr

  @library
  case class Second(e: Expr) extends Expr

  // Functions
  @library
  case class Lambda(x: Variable, ty: Type, e: Expr) extends Expr

  @library
  case class Apply(e1: Expr, e2: Expr) extends Expr

  @library
  case class Rec(f: Variable, x: Variable, tyx: Type, ty: Type, e: Expr) extends Expr

  // Values
  @library
  sealed abstract class Value

  @library
  case class NumV(n: BigInt) extends Value

  @library
  case class BoolV(n: Boolean) extends Value

  @library
  case class StringV(s: BigInt) extends Value

  @library
  case class PairV(v1: Value, v2: Value) extends Value

  @library
  case class ClosureV(env: Env[FakeValue], x: Variable, e: Expr) extends Value

  @library
  case class RecV(env: Env[FakeValue], f: Variable, x: Variable, e: Expr) extends Value

  @library
  case class FakeValue()

  @extern
  @pure
  implicit def value2FakeValue(v: Value): FakeValue = {
    errorWrapper[FakeValue]
  }

  @extern
  @pure
  implicit def fakeValue2Value(v: FakeValue): Value = errorWrapper[Value]

  @extern
  @pure
  implicit def varValue2varFakeValue(v: (Variable, Value)): (Variable, FakeValue) = {
    errorWrapper[(Variable, FakeValue)]
  }

  // Types
  @library
  sealed abstract class Type

  @library
  case object IntTy extends Type

  @library
  case object BoolTy extends Type

  @library
  case object StringTy extends Type

  @library
  case class PairTy(ty1: Type, ty2: Type) extends Type

  @library
  case class FunTy(ty1: Type, ty2: Type) extends Type

  @extern
  @pure
  object Gensym {
    def gensym(s: Variable): Variable = errorWrapper[Nothing]
  }

  @extern
  @pure
  def swapVar(x: Variable, y: Variable, z: Variable): Variable = errorWrapper[Nothing]

  @extern
  @pure
  def eplSwap(e: Expr, y: Variable, z: Variable): Expr = errorWrapper[Nothing]
}