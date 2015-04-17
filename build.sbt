lazy val root = project.in (file("."))  aggregate ( common
                                                  , testApp
                                                  )

lazy val common = project in file("common")

lazy val testApp = project in file("test-app") dependsOn common

// root settings
name := "comp-vis"

publishArtifact := false

CommonSettings.settings