package contracts

import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

import Contracts.Language._ // getting the DSL imported language in
import Contracts.Templates._
import Contracts.Dates._

class ResourceTestSpec extends FreeSpec with Matchers with PropertyChecks {
  // still need to work on these.
  // What are good ways to do property tests on Contracts?

  "Tests on Language" - {
    val buyer = Agent("Jonas")
    val seller = Agent("Hugo")
    val item = Item("bike")
    val amountPaid = 3499
    val t1 = Transaction(buyer,seller, item, Date())
    val t2 = Transaction(buyer,seller, MonetaryValue(amountPaid), Date())
    val t3 = Transaction(seller,buyer, item, Date())

  }
}
