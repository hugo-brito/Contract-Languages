package contracts

import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

import contracts.Resource._ // getting the DSL imported language in

class ResourceTestSpec extends FreeSpec with Matchers with PropertyChecks {
  // still need to work on these.
  // What are good ways to do property tests on Contracts?
}
