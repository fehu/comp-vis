package feh.tec.cvis.db

import feh.tec.cvis.common.cv.describe.CallHistory.{TypedArgEntry, StringArgEntry}
import feh.tec.cvis.common.cv.describe.{ArgDescriptor, CallHistory}

import scala.reflect.ClassTag

case class HistoryArgSerialized(name: String, tag: String, value: String)

object HistoryArgSerialized{
  def create(entry: CallHistory.ArgEntry[_]): HistoryArgSerialized =
    HistoryArgSerialized(entry.arg.name, entry.arg.tag.toString(), entry.stringValue)

  def build(sArg: HistoryArgSerialized): CallHistory.ArgEntry[_] = {
    val (className, valueOpt) = sArg.tag match {
      case "Int"                      => "scala.Int"    -> Some(sArg.value.toInt)
      case "Double"                   => "scala.Double" -> Some(sArg.value.toDouble)
      case c@"scala.math.BigDecimal"  => c              -> Some(BigDecimal(sArg.value))
      case another                    => another        -> None

    }

    val clazz = getClass.getClassLoader.loadClass(className)

    valueOpt.map(v =>   TypedArgEntry (ArgDescriptor(sArg.name, null)(ClassTag(clazz)), v) )
            .getOrElse( StringArgEntry(ArgDescriptor(sArg.name, null)(ClassTag(clazz)), sArg.value) )

  }
}