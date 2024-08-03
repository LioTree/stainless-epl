package stainless
package epl
package assn2

import stainless.lang.*
import stainless.annotation.*

package object framework {
  type Variable = StringWrapper
  type Env[A] = Map[Variable, A]

  // Arithmetic expressions

  sealed abstract class Expr

  case class Num(n: BigInt) extends Expr

  case class Plus(e1: Expr, e2: Expr) extends Expr

  case class Minus(e1: Expr, e2: Expr) extends Expr

  case class Times(e1: Expr, e2: Expr) extends Expr

  // Booleans
  case class Bool(n: Boolean) extends Expr

  case class Eq(e1: Expr, e2: Expr) extends Expr

  case class IfThenElse(e: Expr, e1: Expr, e2: Expr) extends Expr

  // Strings
  case class Str(s: StringWrapper) extends Expr

  case class Length(e: Expr) extends Expr

  case class Index(e1: Expr, e2: Expr) extends Expr

  case class Concat(e1: Expr, e2: Expr) extends Expr

  // Variables and let-binding
  case class Var(x: Variable) extends Expr

  case class Let(x: Variable, e1: Expr, e2: Expr) extends Expr

  case class LetFun(f: Variable, arg: Variable, ty: Type, e1: Expr, e2: Expr)
    extends Expr

  case class LetRec(f: Variable, arg: Variable, xty: Type, ty: Type, e1: Expr, e2: Expr)
    extends Expr

  case class LetPair(x: Variable, y: Variable, e1: Expr, e2: Expr) extends Expr

  // Pairing
  case class Pair(e1: Expr, e2: Expr) extends Expr

  case class First(e: Expr) extends Expr

  case class Second(e: Expr) extends Expr

  // Functions
  case class Lambda(x: Variable, ty: Type, e: Expr) extends Expr

  case class Apply(e1: Expr, e2: Expr) extends Expr

  case class Rec(f: Variable, x: Variable, tyx: Type, ty: Type, e: Expr) extends Expr

  // Values
  sealed abstract class Value

  case class NumV(n: BigInt) extends Value

  case class BoolV(n: Boolean) extends Value

  case class StringV(s: StringWrapper) extends Value

  case class PairV(v1: Value, v2: Value) extends Value

  case class ClosureV(env: Env[FakeValue], x: Variable, e: Expr) extends Value

  case class RecV(env: Env[FakeValue], f: Variable, x: Variable, e: Expr) extends Value

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
  sealed abstract class Type

  case object IntTy extends Type

  case object BoolTy extends Type

  case object StringTy extends Type

  case class PairTy(ty1: Type, ty2: Type) extends Type

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
  def swap(e: Expr, y: Variable, z: Variable): Expr = errorWrapper[Nothing]
}