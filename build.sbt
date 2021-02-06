val scala3Version = "3.0.0-M3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "scala3-simple",
    version := "0.1.0",

    scalaVersion := scala3Version,

   libraryDependencies ++= Seq(
"org.scalatest" % "scalatest_3.0.0-M3" % "3.2.3" % Test, 
"org.scalatestplus" %% "scalacheck-1-15" % "3.2.3.0" % Test
)  
  )
