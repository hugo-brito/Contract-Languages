abstract case class Event() {}

case class Order() extends Event{}

case class Delivery() extends Event{}

case class Payment() extends Event{}


// inventory
// maps string ("item" -> price)
// we could make this smarter and map string "item name" -> item object

//