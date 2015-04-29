package feh.tec.cvis.testapp

import java.awt.{Color, Dimension}
import java.awt.image._
import feh.tec.cvis.common.Helper._
import feh.tec.cvis.common._
import feh.tec.cvis.gui.GenericSimpleApp.DefaultApp
import feh.tec.cvis.gui.configurations.{GuiArgModifier, Harris}
import feh.util._
import org.opencv.core.{Core, Mat}
import scala.swing.Component
import scala.swing.Swing._
import Drawing._

object TestHarris extends DefaultApp("harris-test", 300 -> 300, 600 -> 800) with Harris{

  System.loadLibrary(Core.NATIVE_LIBRARY_NAME)

  def mkSimpleFrame(image: BufferedImage,
                    frameTitle: String,
                    defaultSize: Dimension,
                    regNewFrame: SimpleFrame => Unit,
                    unregAFrame: SimpleFrame => Unit ) =
    new SimpleFrame(image, frameTitle, defaultSize, regNewFrame, unregAFrame)
      with ConfigurationsPanelBuilder
      with HarrisGUI
      with FrameExec
      with CornerDetection
      with MatSupport
      with ColorConverting
    {
      frame =>

      type Config = SimpleVerticalPanel with HarrisConfigurationPanelExec

      lazy val configurations = new SimpleVerticalPanel with HarrisConfigurationPanelExec{
        lazy val elems: Map[String, Seq[Component with UpdateInterface]] =
          formBuilders.mapValues{
            case (form, label) => label.formMeta.form :: form.formMeta.form :: Nil
          }

        def kStep = Some(GuiArgModifier.Step(0.001))

//        override lazy val runner: Runner[Params, Mat, Mat] = Runner(
//          params =>
//            CallDescriptor.WithScopeAndParams(ConvertColor.Descriptor, frame,
//                                              (ColorConversion(BufferedImageColor.mode(modifiedImage), ColorMode.Gray), None)) chain
//            CallDescriptor.WithScopeAndParams(Harris.Descriptor, frame,
//                                              (blockSize, kSize, k.toDouble, Option(borderType)))
//        )

        def cornerResponse: EigenValsAndVecs => Double    =   eign => eign.det / math.pow(eign.trace, 2)
        def isCorner      : EigenValsAndVecs => Boolean   =   eign => math.abs(cornerResponse(eign)) >= 1


        override lazy val runner: Runner[Params, Mat, Mat] = Runner {
          params =>
            src =>
              val cvt = ColorConversion(BufferedImageColor.mode(modifiedImage), ColorMode.Gray)
              cvtColor(src, cvt) |> {
                grayImg =>
                  val eigns = cornerEigenValsAndVecs(grayImg, blockSize, kSize, Option(borderType))
                  println("eigns.length = " + eigns.length)

                  val sorted    = eigns.lazyPairs.mapVals(cornerResponse).sortBy(_._2)
                  val n = sorted.length
                  val filtered  = sorted.take(n/20) ++ sorted.takeRight(n/20)

                  println("filtered = " + filtered.mkString("\t", "\n\t", ""))
//                  println("filtered = " + eigns.flatten.map(cornerReaction andThen math.abs).filter(_ > 10).sorted.mkString(", "))

                  grayImg.convert(cvt.inverse) $${
                    res =>
                      filtered.foreach{
                                        case ((i, j), r) =>
                                          val radious = r.toInt match {
                                            case 0          => 1
                                            case x if x < 0 => 5
                                            case x          => 10
                                          }
                                          res.draw.circle(i -> j, radious, Color.red)
                                      }
                  }
              }

        }

      }

      frame.updateForms()
    }
}
