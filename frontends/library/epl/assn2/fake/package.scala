package epl
package assn2

import epl.assn2.framework.Others.*
import stainless.lang.*
import stainless.annotation.*

package object fake {
  @extern
  @pure
  object Value {
    // utility methods for operating on values
    def add(v1: Value, v2: Value): Value = errorWrapper[Nothing]

    def subtract(v1: Value, v2: Value): Value = errorWrapper[Nothing]

    def multiply(v1: Value, v2: Value): Value = errorWrapper[Nothing]

    def eq(v1: Value, v2: Value): Value = errorWrapper[Nothing]

    def length(v: Value): Value = errorWrapper[Nothing]

    def index(v1: Value, v2: Value): Value = errorWrapper[Nothing]

    def concat(v1: Value, v2: Value): Value = errorWrapper[Nothing]
  }


  @extern
  @pure
  def fake_eval(env: Env[FakeValue], e: Expr): Value = errorWrapper[Nothing]

  @extern
  @pure
  def fake_tyOf(ctx: Env[Type], e: Expr): Type = errorWrapper[Nothing]

  @extern
  @pure
  def fake_subst(t: Expr, e: Expr, x: Variable): Expr = errorWrapper[Nothing]

  @extern
  @pure
  def subst(t: Expr, e: Expr, x: Variable): Expr = errorWrapper[Nothing]

  @extern
  @pure
  def fake_desugar(e: Expr): Expr = errorWrapper[Nothing]
}