ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.6.1"

lazy val scalaTestVersion = "3.2.19"

lazy val root = (project in file("."))
  .settings(
    name := "advent2024",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
      "org.scalatest" %% "scalatest-flatspec" % scalaTestVersion % Test,
    ),
    scalacOptions ++= Seq(
      "-encoding",
      "utf8",
      "-deprecation",
      "-feature",
      "-unchecked",
      "-language:noAutoTupling",
      "-language:strictEquality",
      "-Wunused:all",
      "-Wshadow:all",
      "-Wvalue-discard",
      "-Wsafe-init",
      "-Yexplicit-nulls",
    ),
  )
