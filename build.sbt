name := "ScuttleButtToPadagraph"

version := "0.1"

scalaVersion := "2.13.0"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core" % "0.12.0-M3",
  "io.circe" %% "circe-generic" % "0.12.0-M3",
  "io.circe" %% "circe-parser" % "0.12.0-M3",
  "com.github.tototoshi" %% "scala-csv" % "1.3.6"
)