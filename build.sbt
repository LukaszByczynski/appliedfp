name := "appliedfp"

version := "0.1"

scalaVersion := "2.12.6"

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7")

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.25"