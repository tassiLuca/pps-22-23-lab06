ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.2"

val junitTest = "com.github.sbt" % "junit-interface" % "0.13.3" % Test
val scalaTest = "org.scalatest" %% "scalatest" % "3.2.15" % Test

lazy val root = (project in file("."))
  .settings(
    name := "pps-code-lab-6",
    libraryDependencies ++= Seq(junitTest, scalaTest)
  )
