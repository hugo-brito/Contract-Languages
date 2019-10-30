package contractLanguages

import java.util.Date


abstract case class Event(agent: Agent, timeStamp: Date) {

}

case class Order() extends Event{}
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

case class Delivery() extends Event{}

case class Payment() extends Event{}

// inventory
// maps string ("item" -> price)
// we could make this smarter and map string "item name" -> item object

//