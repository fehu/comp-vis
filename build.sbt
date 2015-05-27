lazy val root = project.in (file("."))  aggregate ( common
                                                  , gui
                                                  , harrisApp
                                                  )

lazy val common = project in file("common")

lazy val gui = project in file("gui") dependsOn common

lazy val harrisApp = project in file("harris-app") dependsOn gui

// root settings
name := "comp-vis"

publishArtifact := false

CommonSettings.settings