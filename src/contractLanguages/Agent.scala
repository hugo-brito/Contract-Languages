package contractLanguages

/*
	Intended functionality:
	Agents originate events, they're parties in the contracts

 */

sealed abstract case class Agent()
// sealed: can only be extended this THIS file
// abstract: cannot be instantiated
// case: many out of the box functions such as hashCode, toString.. etc

case class Supplier()

case class