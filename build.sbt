val scala3Version = "3.0.0-RC2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "S-99",
    version := "0.1.0",

    scalaVersion := scala3Version,

   libraryDependencies ++= Seq(
     "org.scalatest" %% "scalatest" % "3.2.7" % Test,
     "org.scalacheck" %% "scalacheck" % "1.15.3" % Test
    )
  )
