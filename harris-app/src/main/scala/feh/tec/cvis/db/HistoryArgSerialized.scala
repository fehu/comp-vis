package feh.tec.cvis.db

import feh.tec.cvis.common.cv.describe.CallHistory

case class HistoryArgSerialized(name: String, tag: String, value: String)

object HistoryArgSerialized{
  def create(entry: CallHistory.ArgEntry[_]): HistoryArgSerialized =
    HistoryArgSerialized(entry.arg.name, entry.arg.tag.toString(), entry.value.toString)

  def build(sArg: HistoryArgSerialized): CallHistory.ArgEntry[_] = ???
}