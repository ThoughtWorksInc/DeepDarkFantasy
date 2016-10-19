organization in ThisBuild := "com.thoughtworks.deepdarkfantasy"

name := "DeepDarkFantasy"

scalaVersion := "2.11.8"

scalacOptions += "-language:higherKinds"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.4"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.0")
