
inThisBuild(
  Seq(
    scalaVersion      := "3.5.0",
    version           := "0.0.1",
    licenses += ("MIT" -> url("https://opensource.org/licenses/MIT")),
    publishTo         := Option(Resolver.file("file", new File(sys.props.getOrElse("publishTo", "")))),
    semanticdbEnabled := true, // for scalafix
    Compile / packageDoc / publishArtifact := false
  )
)

val commonSettings = Seq(
  scalacOptions := Seq(
    "-encoding",
    "utf-8",
    "-rewrite",
    "-source:3.7-migration",
    // "-indent",
    "-feature",
    "-language:postfixOps",
    "-Wunused:all",
    "-release:22"
    // "-Werror"
    // Warnings as errors!
    /* "-Xfatal-warnings" */
  )
)

lazy val scala = (project in file("scala"))
  .settings(
    commonSettings,
    name := "scala",
    libraryDependencies ++= List(
      "org.scalatest" %% "scalatest" % "3.2.19" % Test,
      "org.scalameta" %% "munit" % "1.0.1" % Test,
      "org.scalameta" %% "munit-scalacheck" % "1.0.0" % Test,
    )
  )

lazy val root = project
  .in(file("."))
  .settings(publish := {}, publish / skip := true)
  .aggregate(scala)
