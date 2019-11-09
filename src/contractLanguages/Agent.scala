package contractLanguages

/*
	Intended functionality:
	Agents originate events, they're parties in the contracts

 */

sealed abstract case class Agent(name: String)
// sealed: can only be extended this THIS file
// abstract: cannot be instantiated
// case: many out of the box functions such as hashCode, toString.. etc

// the constructor takes a name and a taxNum.

case class Supplier(override val name: String, override val taxNum: Int, inventory: Map[String, Int]) extends Agent(name, taxNum) {
	// has an inventory of goods to sell
	def apply(name: String, taxNum: Int, inventory: Map[String, Int]): Supplier = new Supplier(agent, timeStamp, recipient, item): Supplier
}


case class Buyer(override val name: String) extends Agent(name, taxNum)
// has money to pay for goods