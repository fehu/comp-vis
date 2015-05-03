package feh.tec.cvis.common.cv

import feh.tec.cvis.common.cv.Helper._
import org.opencv.core.{Core, Mat, Point}

trait Clustering {

  sealed abstract class CentersPolicy(val value: Int)
  object CentersPolicy{
    /** Select random initial centers in each attempt */
    object Random extends CentersPolicy(Core.KMEANS_RANDOM_CENTERS){ override def toString = "Random" }

    /**  Use <code>kmeans++</code> center initialization by Arthur and Vassilvitskii [Arthur2007]. */
    object PP extends CentersPolicy(Core.KMEANS_PP_CENTERS) { override def toString = "PP" }

    /**
     * During the first (and possibly the only)
     * attempt, use the user-supplied labels instead of computing them from the
     * initial centers. For the second and further attempts, use the random or
     * semi-random centers. Use one of <code>KMEANS_*_CENTERS</code> flag to specify
     * the exact method.
     */
    object InitialLabels extends CentersPolicy(Core.KMEANS_USE_INITIAL_LABELS)
  }

  // C++:  double kmeans(Mat data, int K, Mat& bestLabels, TermCriteria criteria, int attempts, int flags, Mat& centers = Mat())
  /**
   * <p>Finds centers of clusters and groups input samples around the clusters.</p>
   *
   * <p>The function <code>kmeans</code> implements a k-means algorithm that finds
   * the centers of <code>cluster_count</code> clusters and groups the input
   * samples around the clusters. As an output, <em>labels_i</em> contains a
   * 0-based cluster index for the sample stored in the <em>i^(th)</em> row of the
   * <code>samples</code> matrix.</p>
   *
   * <p>The compactness measure that is computed as</p>
   *
   * <p><em>sum _i|samples _i - centers _(labels _i)| ^2</em></p>
   *
   * <p>after every attempt. The best (minimum) value is chosen and the corresponding
   * labels and the compactness value are returned by the function.
   * Basically, you can use only the core of the function, set the number of
   * attempts to 1, initialize labels each time using a custom algorithm, pass
   * them with the (<code>flags</code> = <code>KMEANS_USE_INITIAL_LABELS</code>)
   * flag, and then choose the best (most-compact) clustering.</p>
   *
   * <p>Note:</p>
   * <ul>
   *   <li> An example on K-means clustering can be found at opencv_source_code/samples/cpp/kmeans.cpp
   *   <li> (Python) An example on K-means clustering can be found at
   * opencv_source_code/samples/python2/kmeans.py
   * </ul>
   *
   * @param data Data for clustering. An array of N-Dimensional points with float
   * coordinates is needed. Examples of this array can be:
   * <ul>
   *   <li> <code>Mat points(count, 2, CV_32F);</code>
   *   <li> <code>Mat points(count, 1, CV_32FC2);</code>
   *   <li> <code>Mat points(1, count, CV_32FC2);</code>
   *   <li> <code>std.vector<cv.Point2f> points(sampleCount);</code>
   * </ul>
   * @param k Number of clusters to split the set by.
   * @param criteria The algorithm termination criteria, that is, the maximum
   * number of iterations and/or the desired accuracy. The accuracy is specified
   * as <code>criteria.epsilon</code>. As soon as each of the cluster centers
   * moves by less than <code>criteria.epsilon</code> on some iteration, the
   * algorithm stops.
   * @param attempts Flag to specify the number of times the algorithm is executed
   * using different initial labellings. The algorithm returns the labels that
   * yield the best compactness (see the last function parameter).
   * @param centersPolicy CentersPolicy
   * @return KMeansResult: Cluster centers, best labels and compactness measure.
   *
   * @see <a href="http://docs.opencv.org/modules/core/doc/clustering.html#kmeans">org.opencv.core.Core.kmeans</a>
   */
  def kmeans(data: Mat, 
             k: Int,
             criteria: TerminationCriteria,
             attempts: Int,
             centersPolicy: CentersPolicy,
             labels: Mat = null): KMeansResult = {
    val centers = new Mat() //  Output matrix of the cluster centers, one row per each cluster center.
    val bestLabels = Option(labels).getOrElse(new Mat())

    val compactness = Core.kmeans(data, k, bestLabels, criteria, attempts, centersPolicy.value, centers)
    val stream = centers.byRow(row => _.toArray[Float]: Point )

    KMeansResult(stream.toList, bestLabels, compactness)
   }


  case class KMeansResult(centers: List[Point], bestLabels: Mat, compactness: Double){
    def isEmpty = bestLabels == null && centers.isEmpty
  }
  object KMeansResult{
    def empty = KMeansResult(Nil, null, 0)
  }

}
