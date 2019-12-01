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
    val item = Item("bike", 3499)
    val amountPaid = 3499
    val t1 = Transaction(buyer,seller, item, Date())
    val t2 = Transaction(buyer,seller, MonetaryValue(amountPaid), Date())
    val t3 = Transaction(seller,buyer, item, Date())

    // def sale 
    val order_del = Commitment(t => {if (t.instigator == buyer && t.recipient == seller && t.resource == item) Some(Succ) else None
    })

    val c1 = singleItemSale(buyer, seller, item, amountPaid)
    val c2 = evalC (t1) (c1)
    val c3 = evalC (t2) (c2)

    "(C1 or C2) then C3 = (C1 then C3) or (C2 then C3)" in {
      ((c1 || c2) followedBy c3) shouldBe ((c1 followedBy c3) || (c2 followedBy c3)) 
    }
  }
}
