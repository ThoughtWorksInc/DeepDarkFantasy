organization in ThisBuild := "com.thoughtworks.deepdarkfantasy"

name := "DeepDarkFantasy"

scalaVersion := "2.12.0"

scalacOptions += "-language:higherKinds"
scalacOptions += "-language:existentials"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.7"
libraryDependencies += "org.scalaz" %% "scalaz-effect" % "7.2.7"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
