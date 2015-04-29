package feh.tec.cvis.common.describe

abstract class CallDescriptor[Scope, Input, Params, Result](val call: Scope => Input => Params => Result)

object CallDescriptor{
  class WithScopeAndParams[Input, Result] protected (descr: CallDescriptor[_, Input, _, Result],
                                                     scope: Any,
                                                     params:  Any){
    def call: Input => Result =
      inp =>
        descr.asInstanceOf[CallDescriptor[Any, Input, Any, Result]].call(scope)(inp)(params)
  }

  object WithScopeAndParams{
    def apply[Scope, Input, Params, Result](descr: CallDescriptor[Scope, Input, Params, Result],
                                            scope: Scope,
                                            params:  Params) =
      new WithScopeAndParams[Input, Result](descr, scope, params)
  }

  def chain[Inp, Med, Res](d1: WithScopeAndParams[Inp, Med], d2: WithScopeAndParams[Med, Res]): Inp => Res =
    d2.call compose d1.call

  implicit class ChainWrapper[Inp, Med](d1: WithScopeAndParams[Inp, Med]){
    def chain[Res](d2: WithScopeAndParams[Med, Res]) = CallDescriptor.chain(d1, d2)
  }
}
