package feh.tec.cvis.common.cv

import java.awt.Color

import org.opencv.core._
import org.opencv.imgproc.Imgproc

import scala.language.implicitConversions

object Drawing extends Drawing

trait Drawing {
  implicit def awtColorIsCvScalar(c: Color): Scalar = new Scalar(c.getRed, c.getGreen, c.getBlue)


  abstract class LineType(val value: Int)
  object LineType{
    case object Conn8   extends LineType(Core.LINE_8)
    case object Conn4   extends LineType(Core.LINE_4)
    case object ConnAA  extends LineType(Core.LINE_AA)
  }

  implicit class MatDrawWrapper(mat: Mat){
    def draw: MatDraw = new MatDraw { def img = mat }
  }

  trait MatDraw{
    def img: Mat

    // C++:  void line(Mat& img, Point pt1, Point pt2, Scalar color, int thickness = 1, int lineType = LINE_8, int shift = 0)
    /**
     * <p>Draws a line segment connecting two points.</p>
     *
     * If <code>arrowTipLength</code> is defined, the line will be pointing from the first point to the second one
     *
     * <p>The function <code>line</code> draws the line segment between
     * <code>pt1</code> and <code>pt2</code> points in the image. The line is
     * clipped by the image boundaries. For non-antialiased lines with integer
     * coordinates, the 8-connected or 4-connected Bresenham algorithm is used.
     * Thick lines are drawn with rounding endings.
     * Antialiased lines are drawn using Gaussian filtering. To specify the line
     * color, you may use the macro <code>CV_RGB(r, g, b)</code>.</p>
     *
     * @param pt1 First point of the line segment.
     * @param pt2 Second point of the line segment.
     * @param color Line color.
     * @param thickness Line thickness.
     * @param lineType Type of the line
     * @param shift Number of fractional bits in the point coordinates.
     * @param arrowTipLength Optional: The length of the arrow tip in relation to the arrow length
     *
     * @see <a href="http://docs.opencv.org/modules/core/doc/drawing_functions.html#line">org.opencv.core.Core.line</a>
     * @see <a href="http://docs.opencv.org/modules/core/doc/drawing_functions.html#arrowedline">org.opencv.core.Core.arrowedLine</a>
     */
    def line(pt1: Point,
             pt2: Point,
             color: Scalar,
             thickness: Int                 = 1,
             lineType: LineType             = LineType.Conn8,
             shift: Int                     = 0,
             arrowTipLength: Option[Double] = None
              ) =
      arrowTipLength.map( Imgproc.arrowedLine(img, pt1, pt2, color, thickness, lineType.value, shift, _) )
      .getOrElse( Imgproc.line       (img, pt1, pt2, color, thickness, lineType.value, shift) )


    // C++:  void rectangle(Mat& img, Point pt1, Point pt2, Scalar color, int thickness = 1, int lineType = LINE_8, int shift = 0)
    /**
     * <p>Draws a simple, thick, or filled up-right rectangle.</p>
     *
     * <p>The function <code>rectangle</code> draws a rectangle outline or a filled
     * rectangle whose two opposite corners are <code>pt1</code> and
     * <code>pt2</code>, or <code>r.tl()</code> and <code>r.br()-Point(1,1)</code>.</p>
     *
     * @param pt1 Vertex of the rectangle.
     * @param pt2 Vertex of the rectangle opposite to <code>pt1</code>.
     * @param color Rectangle color or brightness (grayscale image).
     * @param thickness Thickness of lines that make up the rectangle. Negative
     * values, like <code>CV_FILLED</code>, mean that the function has to draw a
     * filled rectangle.
     * @param lineType Type of the line.
     * @param shift Number of fractional bits in the point coordinates.
     *
     * @see <a href="http://docs.opencv.org/modules/core/doc/drawing_functions.html#rectangle">org.opencv.core.Core.rectangle</a>
     */
    def rectangle(pt1: Point,
                  pt2: Point,
                  color: Scalar,
                  thickness: Int      = 1,
                  lineType: LineType  = LineType.Conn8,
                  shift: Int          = 0
                   ) =
      Imgproc.rectangle(img, pt1, pt2, color, thickness, lineType.value, shift)


    // C++:  void circle(Mat& img, Point center, int radius, Scalar color, int thickness = 1, int lineType = LINE_8, int shift = 0)
    /**
     * <p>Draws a circle.</p>
     *
     * <p>The function <code>circle</code> draws a simple or filled circle with a given
     * center and radius.</p>
     *
     * @param center Center of the circle.
     * @param radius Radius of the circle.
     * @param color Circle color.
     * @param thickness Thickness of the circle outline, if positive. Negative
     * thickness means that a filled circle is to be drawn.
     * @param lineType Type of the circle boundary.
     * @param shift Number of fractional bits in the coordinates of the center and
     * in the radius value.
     *
     * @see <a href="http://docs.opencv.org/modules/core/doc/drawing_functions.html#circle">org.opencv.core.Core.circle</a>
     */
    def circle(center: Point,
               radius: Int,
               color: Scalar,
               thickness: Int      = 1,
               lineType: LineType  = LineType.Conn8,
               shift: Int          = 0
                ) =
      Imgproc.circle(img, center, radius, color, thickness, lineType.value, shift)


    // C++:  void ellipse(Mat& img, Point center, Size axes, double angle, double startAngle, double endAngle, Scalar color, int thickness = 1, int lineType = LINE_8, int shift = 0)
    /**
     * <p>Draws a simple or thick elliptic arc or fills an ellipse sector.</p>
     *
     * <p>The functions <code>ellipse</code> with less parameters draw an ellipse
     * outline, a filled ellipse, an elliptic arc, or a filled ellipse sector.
     * A piecewise-linear curve is used to approximate the elliptic arc boundary. If
     * you need more control of the ellipse rendering, you can retrieve the curve
     * using "ellipse2Poly" and then render it with "polylines" or fill it with
     * "fillPoly". If you use the first variant of the function and want to draw the
     * whole ellipse, not an arc, pass <code>startAngle=0</code> and
     * <code>endAngle=360</code>. The figure below explains the meaning of the
     * parameters.
     * Figure 1. Parameters of Elliptic Arc</p>
     *
     * @param center Center of the ellipse.
     * @param axes Half of the size of the ellipse main axes.
     * @param angle Ellipse rotation angle in degrees.
     * @param startAngle Starting angle of the elliptic arc in degrees.
     * @param endAngle Ending angle of the elliptic arc in degrees.
     * @param color Ellipse color.
     * @param thickness Thickness of the ellipse arc outline, if positive.
     * Otherwise, this indicates that a filled ellipse sector is to be drawn.
     * @param lineType Type of the ellipse boundary.
     * @param shift Number of fractional bits in the coordinates of the center and
     * values of axes.
     *
     * @see <a href="http://docs.opencv.org/modules/core/doc/drawing_functions.html#ellipse">org.opencv.core.Core.ellipse</a>
     */
    def ellipse(center: Point,
                axes: Size,
                angle: Double,
                startAngle: Double,
                endAngle: Double,
                color: Scalar,
                thickness: Int      = 1,
                lineType: LineType  = LineType.Conn8,
                shift: Int          = 0
                 ) =
      Imgproc.ellipse(img, center, axes, angle, startAngle, endAngle, color, thickness, lineType.value, shift)


    // C++:  void fillConvexPoly(Mat& img, vector_Point points, Scalar color, int lineType = LINE_8, int shift = 0)
    /**
     * <p>Fills a convex polygon.</p>
     *
     * <p>The function <code>fillConvexPoly</code> draws a filled convex polygon.
     * This function is much faster than the function <code>fillPoly</code>. It can
     * fill not only convex polygons but any monotonic polygon without
     * self-intersections, that is, a polygon whose contour intersects every
     * horizontal line (scan line) twice at the most (though, its top-most and/or
     * the bottom edge could be horizontal).</p>
     *
     * @param points a points
     * @param color Polygon color.
     * @param lineType Type of the polygon boundaries.
     * @param shift Number of fractional bits in the vertex coordinates.
     *
     * @see <a href="http://docs.opencv.org/modules/core/doc/drawing_functions.html#fillconvexpoly">org.opencv.core.Core.fillConvexPoly</a>
     */
    def fillConvexPoly(points: MatOfPoint,
                       color: Scalar,
                       lineType: LineType  = LineType.Conn8,
                       shift: Int          = 0
                        ) =
      Imgproc.fillConvexPoly(img, points, color, lineType.value, shift)



    // todo: more if needed
  }

}
