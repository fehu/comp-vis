lazy val root = project.in (file("."))  aggregate ( common
                                                  , harrisApp
                                                  )

lazy val common = project in file("common")

lazy val harrisApp = project in file("harris-app") dependsOn common

// root settings
name := "comp-vis"

publishArtifact := false

CommonSettings.settings