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
  def empty[T](value: T): CallHistoryContainer[T] = CallHistoryContainer(value, CallHistory.Empty)
}

object CallHistory{

  object Empty extends CallHistory(null, Nil){
    override def aggregate[Res](entry: Entry[Res]): CallHistory[Res] = CallHistory(entry, Nil)
  }
  
  trait ArgEntry[T] {
    def arg: ArgDescriptor[T]
    def valueOpt: Option[T]
    def stringValue: String
  }
  case class TypedArgEntry[T](arg: ArgDescriptor[T], value: T) extends ArgEntry[T]{
    def stringValue = value.toString
    def valueOpt: Option[T] = Some(value)
  }

  case class StringArgEntry[T](arg: ArgDescriptor[T], stringValue: String) extends ArgEntry[T]{
    def valueOpt: Option[T] = None
  }

  case class Entry[+Res](call: CallDescriptor[Res], args: Set[ArgEntry[_]]){
    def getArg[T](ad: ArgDescriptor[T]): Option[T] = args.find(_.arg == ad).map(_.valueOpt.map(_.asInstanceOf[T])).flatten
    def getStringArg(ad: ArgDescriptor[_]): Option[String] = args.find(_.arg == ad).map(_.stringValue)

    def arg[T]        = getArg[T]     _ andThen (_.get)
    def stringArg[T]  = getStringArg  _ andThen (_.get)
  }
  
  object Entry{
    def apply[R](descr: String): Entry[R] = Entry(CallDescriptor(descr), Set())
  }

}