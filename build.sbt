name := "validated-examples"
organization := "com.gu"
scalaVersion := "3.4.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Datawrapper Import",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.5.4",
    ),
    scalacOptions ++= Seq(
      "-java-output-version",
      "21",
      "-Wunused:all",
      "-Wvalue-discard",
      "-Wnonunit-statement",
      "-Wimplausible-patterns",
      "-unchecked"
    ),
    Compile / run / fork := true,
  )
