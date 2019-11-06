package contractLanguages

/*
	Intended functionality:
	Agents originate events, they're parties in the contracts

 */

sealed abstract case class Agent(name: String, taxNum: Int)
// sealed: can only be extended this THIS file
// abstract: cannot be instantiated
// case: many out of the box functions such as hashCode, toString.. etc

// the constructor takes a name and a taxNum.

case class Supplier(override val name: String, override val taxNum: Int, inventory: Map[String, Int]) extends Agent(name, taxNum)
// has an inventory of goods to sell

case class Buyer(override val name: String, override val taxNum: Int, balanceDKK: Int) extends Agent(name, taxNum)
// has money to pay for goods