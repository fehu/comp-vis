package feh.tec.cvis.common.describe

abstract class CallDescriptor[Scope, Input, Params, Result](val call: Scope => Input => Params => Result)
