import sbt._
import Dependencies._

ThisBuild / scalaVersion := "3.3.0"
ThisBuild / version      := "0.0.1"
ThisBuild / organization := "io.koff"
ThisBuild / scalacOptions ++= Seq(
  "--source:future",
  "-deprecation",
  "-Ykind-projector:underscores"
)

lazy val root = project
  .in(file("."))
  .settings(name := "advanced-scala-tf")
  .aggregate(`context-passing`, `effect-gathering`, `error-handling`)

lazy val `context-passing` = project
  .in(file("context-passing"))
  .settings(libraryDependencies ++= all)

lazy val `effect-gathering` = project
  .in(file("effect-gathering"))
  .settings(libraryDependencies ++= all)

lazy val `lens-derivation` = project
  .in(file("lens-derivation"))
  .settings(libraryDependencies ++= all)

lazy val `error-handling` = project.in(file("error-handling"))
