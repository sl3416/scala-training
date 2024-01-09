lazy val root = Project(id = "functional-patterns", base = file("."))
  .settings(moduleName := "functional-patterns")
  .settings(
    publish         := {},
    publishLocal    := {},
    publishArtifact := false
  )
  .aggregate(json, codecs, parsecs, gen)

lazy val json    = project.dependsOn(parsecs)
lazy val codecs  = project.dependsOn(json)
lazy val parsecs = project
lazy val gen     = project
