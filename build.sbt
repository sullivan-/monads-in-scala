name := "scala-monads"

version := "0.0"

scalaVersion := "2.10.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0.M5b" % "test"

libraryDependencies += "org.easymock" % "easymockclassextension" % "3.1" % "test"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

parallelExecution in Test := false
