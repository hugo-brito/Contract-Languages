package contractLanguages

import java.util.Date

// The prefix format is extended here to associate a name for a particular received event,
// making it possible to formulate a predicate on it. This predicate follows the keyword where,
// and the event is only accepted if it evaluates to True. The BikeOrder event is bound to
// a variable named order, and we use the where clause to check that the fields of order have
// the desired values, using the connective && which is the logical “and” operator. In this
// case, the event order is accepted only if the field order.price is set to 100 and the field
// order.recipient is set to seller. Likewise, the BikeDelivery event is bound to the variable
// delivery and is only accepted if delivery.recipient is buyer.


sealed abstract case class Event(agent: Agent, timeStamp: Date) {
	// sealed class: can only be extended in the present file
	// abstract class: cannot be instantiated, normally used to be extended
	// case class: out-of-the-box methods & other convenient methods

	// the event class is one of the most basic constituents of our contracts.
	// events happen and the contract progresses.

	// a contract is a set of events subsequent.

}

case class Order(override val agent: Agent, override val timeStamp: Date, recipient: Agent, item: String) extends Event(agent, timeStamp) {
	// recipient = the recipient of the order
	// item = the item ordered
	val EventType = "Order"

	def apply(agent: Agent, timeStamp: Date, recipient: Agent, item: String): Order = new Order(agent, timeStamp, recipient, item): Order
	// the apply method allows for an easy way to create new objects
}
/* we want to use case classes because:

	case class Person(name: String, age: Int)
	// 1. class parameters are fields
	val jim = new Person("Jim", 34)
	println(jim.name)

	// 2. sensible toString
	// println(instance) = println(instance.toString) // syntactic sugar
	println(jim.toString)
	println(jim)

	// 3. equals and hashCode implemented out-of-the-box
	val jim2 = new Person("Jim", 34)
	println(jim == jim2) // true

	// 4. case classes have handy copy methods
	val jim3 = jim.copy(age = 45)
	println(jim == jim.copy())
	println(jim3.age)

	// 5. case classes have companion objects
	val thePerson = Person
	val mary = Person("Mary", 23)

	// 6. case classes are serializable
	// Akka

	// 7. case classes have extractor patterns = Case classes can be used in PATTERN MATCHING
*/

case class Delivery(override val agent: Agent, override val timeStamp: Date, recipient: Agent, item: String) extends Event(agent, timeStamp){
	// recipient =  the recipient of the delivery
	val EventType = "Delivery"

	def apply(agent: Agent, timeStamp: Date, recipient: Agent, item: String): Delivery = new Delivery(agent, timeStamp, recipient, item)
}

case class Payment(override val agent: Agent, override val timeStamp: Date, recipient: Agent, amountDKK: Int) extends Event(agent, timeStamp){
	// recipient = the recipient of the payment
	// amountDKK = the amount paid to the recipient
	val EventType = "Payment"

	def apply(agent: Agent, timeStamp: Date, recipient: Agent, amountDKK: Int): Payment = new Payment(agent, timeStamp, recipient, amountDKK)
}

// inventory
// maps string ("item" -> price)
// we could make this smarter and map string "item name" -> item object

//