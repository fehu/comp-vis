import sbt.Keys._
import sbt._

object AssemblySettings{
  lazy val settings = unmanagedResourceDirectories in Compile += { file("libs") }
}