import AssemblyKeys._

name := """ASA"""

version := "1.0"

scalaVersion := "2.11.7"

// Change this to another test framework if you prefer
//libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

// Uncomment to use Akka
//libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.11"

//self changed
libraryDependencies ++= Seq(
"org.scala-lang.modules" %% "scala-xml" % "1.0.5",
"org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
"org.scala-lang.modules" %% "scala-swing" % "1.0.2"
)

unmanagedBase := baseDirectory.value / "libs"

assemblySettings

mainClass in assembly := Some("cl.asa.Asa")
