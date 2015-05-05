package feh.tec.cvis

import javax.swing.table.DefaultTableModel

import feh.dsl.swing2.ComponentExt._
import feh.dsl.swing2.{Monitor, Var}
import feh.tec.cvis.DescriptorsSupport.{ADescriptor, IDescriptor}
import feh.tec.cvis.gui.GenericSimpleAppFrameImplementation
import feh.tec.cvis.gui.configurations.ConfigBuildHelper
import org.opencv.core.Point

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.FiniteDuration
import scala.swing.{Component, ScrollPane, Table}

trait AdminSupport {
  env: GenericSimpleAppFrameImplementation with ConfigBuildHelper =>

  trait AdminSupportFrame extends ConfigurationsPanelBuilder {
    frame: GenericSimpleAppFrame with FrameExec with LayoutDSL with ConfigBuildHelperGUI =>

    trait AdminPanel
      extends SimpleVerticalPanel
      with PanelExec[(Int, Int, Int, Int, Array[Byte], Map[Point, ADescriptor]), IDescriptor]
      with ConfigBuildHelperPanel
    {
      type Params  = String // image name
      def classTag = scala.reflect.classTag[IDescriptor]

      def steps = 1

      def getParams(): Params = imageName.get

      def fetchDbInfo(): Future[Seq[(String, Int)]]

      def dbAccessTimeout: FiniteDuration

      lazy val imageName: Var[String] = Var(null)
      lazy val dbInfo   : Var[Seq[(String, Int)]] = Var(Nil)

      lazy val imageNameControl = controlFor(imageName.get)(imageName.set).textForm
      lazy val listDbTrigger: DSLButtonBuilder = triggerFor{
        listDbTrigger.component.lock()
        val f = fetchDbInfo()
        f onSuccess   { case xs => dbInfo set xs }
        f onComplete  { case _  => listDbTrigger.component.unlock() }
        try Await.ready(f, dbAccessTimeout)
        catch{ case th: Throwable => listDbTrigger.component.unlock(); throw th }
      }.button("List DB entries")


      private def dbInfoModel(data: Seq[(String, Int)]) = {
        val names = "Name" :: "Points of interest" :: Nil
        val dArr = data.toArray.map {
          case (name, count) => Array[AnyRef](name, Int.box(count))
        }
        new DefaultTableModel(dArr, names.toArray[AnyRef]){
          override def isCellEditable(row: Int, column: Int) = false
        }
      }
      lazy val dbInfoMonitor = Monitor.custom(dbInfo, new Table){
        c =>      c.model = dbInfoModel(Nil)
      }{
        c => t => c.model = dbInfoModel(t)
      }

      lazy val formBuilders: Seq[(String, (AbstractDSLBuilder, DSLLabelBuilder[_]))] = Seq(
        "imageName" -> (imageNameControl  -> label("Image name"))
      , "listDb"    -> (listDbTrigger     -> label(null))
      )

      override lazy val elems: Seq[(String, Seq[Component])] = mkElems ++ Seq(
        "dbInfo" -> Seq(new ScrollPane(dbInfoMonitor.component))
      )



      protected def throwIfInterrupted(): Unit = if(interrupted_?) throw Interrupted

      def runner: Runner[Params, (Int, Int, Int, Int, Array[Byte], Map[Point, ADescriptor]), IDescriptor] = Runner(
        nextStep =>
          name => {
            case (width, height, matTpe, javaTpe, imgBytes, descriptorsMap) => IDescriptor(
                                                                            name
                                                                          , descriptorsMap.values.head.sideLength
                                                                          , matTpe
                                                                          , javaTpe
                                                                          , new org.opencv.core.Size(width, height)
                                                                          , imgBytes
                                                                          , descriptorsMap
                                                                          )()
          }
      )
    }
  }
}
