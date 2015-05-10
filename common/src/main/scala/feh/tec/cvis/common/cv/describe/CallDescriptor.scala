package feh.tec.cvis.common.cv.describe

abstract class CallDescriptor[+Result]{
  def name: String

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: CallDescriptor[_] => this.name == that.name
  }
}

object CallDescriptor{
  def apply[Res](nme: String): CallDescriptor[Res] = new CallDescriptor[Res]{ def name = nme }
}

abstract class CallContainer[Scope, Input, Params, Result](val name: String,
                                                           val call: Scope => Input => Params => Result) extends CallDescriptor[Result]


case class CallHistory[+LastRes](last: CallHistory.Entry[LastRes], prev: List[CallHistory.Entry[_]]){
  /** In reversed order */
  def toList = last :: prev

  def aggregate[Res](entry: CallHistory.Entry[Res]): CallHistory[Res] = CallHistory(entry, toList)
}

case class CallHistoryContainer[+R](value: R, history: CallHistory[R]){
  def affect[T](hist: CallHistory.Entry[T])(f: R => T): CallHistoryContainer[T] = copy(f(value), history aggregate hist)
}

object CallHistoryContainer{
  object Empty extends CallHistoryContainer(null, CallHistory.Empty)
}

object CallHistory{

  object Empty extends CallHistory(null, Nil){
    override def aggregate[Res](entry: Entry[Res]): CallHistory[Res] = CallHistory(entry, Nil)
  }
  
  case class ArgEntry[T](arg: ArgDescriptor[T], value: T)
  
  case class Entry[+Res](call: CallDescriptor[Res], args: Set[ArgEntry[_]]){
    def getArg[T](ad: ArgDescriptor[T]): Option[T] = args.find(_.arg == ad).map(_.value.asInstanceOf[T])

    def arg[T] = getArg[T] _ andThen (_.get)
  }

}