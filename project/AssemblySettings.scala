import sbt.Keys._
import sbt._
import sbtassembly.AssemblyKeys._
import sbtassembly.{PathList, MergeStrategy}

object AssemblySettings{
  lazy val settings = Seq(
    unmanagedResourceDirectories in Compile += { file("libs") },
    assemblyForEachTask
    )

  lazy val assemblyForEach = taskKey[Seq[File]]("assembles a jar for each platform")

  lazy val assemblyForEachTask =
    assemblyForEach := {
      val st = state.value
      val jname = (assemblyJarName in assembly).value
      val (name, ext) = jname.splitAt(jname.lastIndexOf('.'))

      def nst(osArch: String) = Project.extract(st).append(Seq(
        assemblyJarName in assembly := {
          name + "_" + osArch + ext
        },
        assemblyMergeStrategy in assembly := {
          case PathList("opencv", `osArch`, xs@_*)  => MergeStrategy.first
          case PathList("opencv", _, xs@_*)         => MergeStrategy.discard
//          case PathList("javax", "servlet", xs@_*) => MergeStrategy.first
//          case PathList(ps@_*) if ps.last endsWith ".html" => MergeStrategy.first
//          case "application.conf" => MergeStrategy.concat
//          case "unwanted.txt" => MergeStrategy.discard
          case x =>
            val oldStrategy = (assemblyMergeStrategy in assembly).value
            oldStrategy(x)
        }),
        st
      )
      for(path <- CommonSettings.supported) yield Project.runTask(assembly, nst(path))
                                                    .get._2
                                                    .toEither.right
                                                    .get
    }
}