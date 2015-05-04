import sbt.Keys._
import sbt._
import com.typesafe.sbt.SbtProguard._

object ProguardSettings{
  lazy val settings = proguardSettings ++ Seq(
      proguardCrossPlatformTask
    , proguardEachPlatformTask
    )

  lazy val proguardCrossPlatform  = taskKey[File]("builds a cross-platform jar")
  lazy val proguardEachPlatform   = taskKey[Seq[File]]("builds a jar for each platform")

  def proguardCrossPlatformTask = proguardCrossPlatform := {
    val st = state.value

    val nst = Project.extract(st).append(Seq(
      ProguardKeys.inputs in Proguard ++= (file("libs") / "opencv").listFiles().toSeq
    ), st)

    Project.runTask(ProguardKeys.proguard in Proguard, nst)
    .getOrElse(sys.error("failed to run task"))._2.toEither
    .left.map(sys error _.toString)
    .right.get.head
  }

  def proguardEachPlatformTask = proguardEachPlatform := {
    val st = state.value

    def nst(path: String) = Project.extract(st).append(Seq(
      ProguardKeys.inputs in Proguard += file("libs") / "opencv" / path
    , artifactName in packageBin := ((s, m, a) =>
          (artifactName in packageBin).value(s, m, a).dropRight(4) + "-" + path + ".jar"
        )
    ), st)

    for(path <- CommonSettings.supported) yield Project.runTask(ProguardKeys.proguard in Proguard, nst(path))
                                                .getOrElse(sys.error("failed to run task"))._2.toEither
                                                .left.map(sys error _.toString)
                                                .right.get.head
  }


  //  lazy val assemblyForEachTask =
//    assemblyForEach := {
//      val st = state.value
//      val jname = (assemblyJarName in assembly).value
//      val (name, ext) = jname.splitAt(jname.lastIndexOf('.'))
//
//      def nst(osArch: String) = Project.extract(st).append(Seq(
//        assemblyJarName in assembly := {
//          name + "_" + osArch + ext
//        },
//        assemblyMergeStrategy in assembly := {
//          case PathList("opencv", `osArch`, xs@_*)  => MergeStrategy.first
//          case PathList("opencv", _, xs@_*)         => MergeStrategy.discard
////          case PathList("javax", "servlet", xs@_*) => MergeStrategy.first
////          case PathList(ps@_*) if ps.last endsWith ".html" => MergeStrategy.first
////          case "application.conf" => MergeStrategy.concat
////          case "unwanted.txt" => MergeStrategy.discard
//          case x =>
//            val oldStrategy = (assemblyMergeStrategy in assembly).value
//            oldStrategy(x)
//        }),
//        st
//      )
//      for(path <- CommonSettings.supported) yield Project.runTask(assembly, nst(path))
//                                                    .get._2
//                                                    .toEither.right
//                                                    .get
//    }
}