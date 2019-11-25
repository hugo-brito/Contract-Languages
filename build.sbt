name := "testing"

scalaVersion := "2.12.9"

scalacOptions += "-deprecation"

scalacOptions += "-feature"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

initialCommands in console := """
import Contracts.Language._
import Contracts.Templates._
import Contracts.Date._
import Contracts.jTemplates._
"""