package stainless.frontends.dotc.epl

import stainless.epl.{optExtractTarget, optFakeExercises, optGenSubFuns, optAssn1, optAssn2}
import stainless.equivchk.optSubFnsEquiv

trait AssnContext(using inoxCtx: inox.Context) {
  protected val targets = inoxCtx.options.findOption(optExtractTarget) match {
    case Some(targets) => Set(targets: _*)
    case None => Set.empty
  }

  protected val fakeExercises = inoxCtx.options.findOption(optFakeExercises) match {
    case Some(targets) => Set(targets: _*)
    case None => Set.empty
  }

  protected val assn1 = inoxCtx.options.findOption(optAssn1) match {
    case Some(true) => true
    case _ => false
  }

  protected val assn2 = inoxCtx.options.findOption(optAssn2) match {
    case Some(true) => true
    case _ => false
  }

  protected val subFnsEquiv = inoxCtx.options.findOption(optSubFnsEquiv) match {
    case Some(true) => true
    case _ => false
  }

  protected val genSubFuns = inoxCtx.options.findOption(optGenSubFuns) match {
    case Some(true) => true
    // When subFnsEquiv is true, we need to generate subFuns even if optGenSubFuns is not set
    case _ => if (subFnsEquiv) true else false
  }
}
