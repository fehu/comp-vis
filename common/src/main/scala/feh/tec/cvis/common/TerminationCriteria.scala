package feh.tec.cvis.common

import org.opencv.core.TermCriteria
import scala.language.implicitConversions

/** Wrapper for [[org.opencv.core.TermCriteria]]
 * <p>class CV_EXPORTS TermCriteria <code></p>
 *
 * <p>// C++ code:</p>
 *
 *
 * <p>public:</p>
 *
 * <p>enum</p>
 *
 *
 * <p>COUNT=1, //!< the maximum number of iterations or elements to compute</p>
 *
 * <p>MAX_ITER=COUNT, //!< ditto</p>
 *
 * <p>EPS=2 //!< the desired accuracy or change in parameters at which the
 * iterative algorithm stops</p>
 *
 * <p>};</p>
 *
 * <p>//! default constructor</p>
 *
 * <p>TermCriteria();</p>
 *
 * <p>//! full constructor</p>
 *
 * <p>TermCriteria(int type, int maxCount, double epsilon);</p>
 *
 * <p>//! conversion from CvTermCriteria</p>
 *
 * <p>TermCriteria(const CvTermCriteria& criteria);</p>
 *
 * <p>//! conversion to CvTermCriteria</p>
 *
 * <p>operator CvTermCriteria() const;</p>
 *
 * <p>int type; //!< the type of termination criteria: COUNT, EPS or COUNT + EPS</p>
 *
 * <p>int maxCount; // the maximum number of iterations/elements</p>
 *
 * <p>double epsilon; // the desired accuracy</p>
 *
 * <p>};</p>
 *
 * <p>The class defining termination criteria for iterative algorithms. You can
 * initialize it by default constructor and then override any parameters, or the
 * structure may be fully initialized using the advanced variant of the
 * constructor.
 * </code></p>
 * @param tpe [[feh.tec.cvis.common.TerminationCriteria.Type]]
 * @param maxCount the maximum number of iterations/elements.
 * @param epsilon the desired accuracy.
 * @see <a href="http://docs.opencv.org/modules/core/doc/basic_structures.html#termcriteria">org.opencv.core.TermCriteria</a>
 */

case class TerminationCriteria(tpe: TerminationCriteria.Type, maxCount: Int, epsilon: Double) {
  lazy val underlying: TermCriteria = new TermCriteria(tpe.value, maxCount, epsilon)
}

object TerminationCriteria{

  def byCount(c: Int) = new TerminationCriteria(Type.Count, c, Double.NaN)
  def byEpsilon(eps: Double) = new TerminationCriteria(Type.Epsilon, -1, eps)
  def apply(c: Int, eps: Double) = new TerminationCriteria(Type.Both, c, eps)

  implicit def terminationCriteriaToCV(tc: TerminationCriteria): TermCriteria = tc.underlying

  sealed abstract class Type(val value: Int)
  object Type{
    /** The maximum number of iterations or elements to compute */
    object Count extends Type(TermCriteria.COUNT)

    /** The maximum number of iterations or elements to compute */
    def MaxIter = Count

    /** The desired accuracy threshold or change in parameters at which the iterative algorithm is terminated. */
    object Epsilon extends Type(TermCriteria.EPS)

    /** COUNT + EPS */
    object Both extends Type(TermCriteria.COUNT + TermCriteria.EPS)
  }
}