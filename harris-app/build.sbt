name := "test-app"

version := "0.0"

CommonSettings.settings

libraryDependencies += "commons-io" % "commons-io" % "2.4" % "compile, runtime"

libraryDependencies += "org.scalanlp" %% "breeze" % "0.11.2"

libraryDependencies += "com.typesafe.slick" %% "slick" % "3.0.0"



AssemblySettings.settings

assemblyJarName in assembly := "test-harris.jar"

mainClass in assembly := Some("feh.tec.cvis.HarrisApp")