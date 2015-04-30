name := "test-app"

version := "0.0"

CommonSettings.settings



assemblyJarName in assembly := "test-harris.jar"

mainClass in assembly := Some("feh.tec.cvis.testapp.TestHarris")