ThisBuild / scalaVersion := "3.3.4"
ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / resolvers ++= Seq(
  "Maven Central" at "https://repo1.maven.org/maven2/",
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"
)

lazy val root = (project in file("."))
  .settings(
    name := "untitled",

    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.18" % Test,

      "ch.qos.logback" % "logback-classic" % "1.5.6",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",

      "com.typesafe" % "config" % "1.4.3"
    ),

    javacOptions ++= Seq("-source", "17", "-target", "17"),
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
      "-Xfatal-warnings"
    )
  )