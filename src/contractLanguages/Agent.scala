package contractLanguages

/*
	Intended functionality:
	Agents originate events, they're parties in the contracts

 */

sealed abstract case class Agent(name: String, taxNum: Int)
// sealed: can only be extended this THIS file
// abstract: cannot be instantiated
// case: many out of the box functions such as hashCode, toString.. etc

case class Supplier()
// has an inventory of goods to sell

case class Buyer()
// has money to pay for goods

case class