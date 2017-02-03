name := "scala-monads"

version := "0.0"

scalaVersion := "2.12.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

libraryDependencies += "org.easymock" % "easymockclassextension" % "3.1" % "test"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

parallelExecution in Test := false
