lazy val commonSettings = Seq(
  organization := "es.umh.uwicore",
  version := "0.0.1",
  scalaVersion := "2.11.4"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "Mano inocente",
    libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.1" % "test"
  )