name := "crypto-hw"

version := "0.1"

scalaVersion := "2.12.7"
scalacOptions += "-Ypartial-unification"
addCompilerPlugin(
  "org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full
)

libraryDependencies += "com.github.mpilquist" %% "simulacrum" % "0.13.0"
libraryDependencies += "org.typelevel" %% "cats-core" % "1.4.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.+" % "test"
