import Dependencies._

ThisBuild / scalaVersion := "2.13.12"
ThisBuild / version := "0.1"

lazy val root = (project in file("."))
  .settings(
    name := "gremlin-step-parser",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
      "com.michaelpollmeier" %% "gremlin-scala" % "3.5.3.7",
      munit % Test
    )
  )

enablePlugins(JavaAppPackaging)
