package feh.tec.cvis

import feh.tec.cvis.common.cv.describe.CallHistory
import feh.tec.cvis.gui.GenericSimpleAppFrameImplementation

import scala.swing.{ScrollPane, Table, Frame}

trait ShowHistorySupport {
  env: GenericSimpleAppFrameImplementation =>

  case class ShowHistoryFrame(hist: CallHistory[_]) extends Frame{
    protected val names = "Call" :: "Arg. Name" :: "Arg. Tag" :: "Arg. Value" :: Nil
    protected val data: Array[Array[Any]]  = hist.toList.flatMap{
      case null => Nil
      case CallHistory.Entry(descr, args) =>
        args.toList.map(a => Array[Any](a.arg.name, a.arg.tag.toString, a.stringValue)) match {
          case Nil => List(descr.name +: Array.fill[Any](3)(""))
          case head :: tail => (descr.name +: head) :: tail.map("" +: _)
        }
    }.toArray

    val table = new Table(data, names)

    contents = new ScrollPane(table)
  }
}
