name := "testing"

scalaVersion := "2.12.9"

scalacOptions += "-deprecation"

scalacOptions += "-feature"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

initialCommands in console := """
import contracts.Language._
import contracts.Templates._
import contracts.Dates._

// Agents, Resources and Basics
val partyA = Agent("Jonas")
val partyB = Agent("Hugo")
val camcorder = Item("camcorder")
val dailyFee = 350.0
val days = 10
val daysLate = 8
// Transactions
val order = Transaction(partyA,partyB, camcorder, Date())
val payment = Transaction(partyA,partyB, MonetaryValue(days*350), Date())
val receiveCamera = Transaction(partyB,partyA, camcorder, Date())
val returnCamera = Transaction(partyA,partyB, camcorder, Date() + Days(8))
val returnLate = Transaction(partyA,partyB,camcorder, Date() + Days(days + daysLate))
val lateFee = Transaction(partyA,partyB, MonetaryValue(daysLate*(days*1.5)), Date())
// Template
val c = rental_agreement(partyA,partyB,camcorder,days,dailyFee)
// Reduction
val c1 = reduce(order) (c)
val c2 = reduce(payment) (c1)
val c3 = reduce(receiveCamera) (c2)
val c4 = reduce(returnCamera) (c3)
val c4_1 = reduce(returnLate) (c3)
val c4_2 = reduce(lateFee) (c4_1)
"""