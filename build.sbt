name := "testing"

scalaVersion := "2.12.9"

scalacOptions += "-deprecation"

scalacOptions += "-feature"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

initialCommands in console := """
import Contracts.Language._
import Contracts.Templates._
import Contracts.Dates._

val buyer = Agent("Jonas")
val seller = Agent("Hugo")
val item = Item("bike", 3499)
val amountPaid = 3499
val t1 = Transaction(buyer,seller, item, Date())
val t2 = Transaction(buyer,seller, MonetaryValue(amountPaid), Date())
val t3 = Transaction(seller,buyer, item, Date())
"""