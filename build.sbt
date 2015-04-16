lazy val root = project.in (file("."))  aggregate common

lazy val common = project in file("common")

// root settings
name := "comp-vis"

publishArtifact := false

CommonSettings.settings