package Contracts

import java.time.LocalDateTime;
import Language._

object Templates extends App {

//     def total(sb: ShoppingCart) : Int = {
//         def rHelper (l: List[Item]) (acc: Int) = 
//             l match {
//                 case Nill => acc
//                 case x::xs => rHelper (xs) (acc + x.value)
//             }
//         rHelper (sb.items) (0)
//     }

//     def multipleShop (buyer: Agent, seller: Agent, res: ShoppingCart) {
//         // val = Atom()
//     }

// //     // val orderBike = Order(10, "bike", jonas, hugo, "today")
// //     // val payBike = Payment(10, hugo, jonas, "today")
// //     // val deliverBiker = Delivery("bike", jonas, hugo, "today")

// //     // val buyBike = orderPayDelivery (hugo) (jonas) (10) ("bike")

// //     // evalC (orderBike) (buyBike)


	// def orderPayDelivery (buyer: Agent) (seller: Agent) (amount: Int) (item: String): Contract = {
    //     val orderEvent = Atom(e => checkOrd (amount) (seller) (item) (e))
    //     val payEvent = Atom(e => checkPay (amount) (buyer) (e))
    //     val delEvent = Atom(e => checkDel (item) (buyer) (e))
    //     Then(orderEvent, Then(payEvent,delEvent))
	// }


	/* Simple contract for the exchange of resources. */
	def singleItemSale (buyer: Agent, seller: Agent, item: Item, amountPaid: Int): Contract = {
		// The transactions the template is expecting
		lazy val order	  = Atom(t => t.instigator == buyer && t.recipient == seller && t.resource == item)
		lazy val payment  = Atom(t => t.instigator == buyer && t.recipient == seller && t.resource == MonetaryValue(amountPaid))
		lazy val delivery = Atom(t => t.instigator == seller && t.recipient == buyer && t.resource == item)
		// The way the tranctions are combined in the given contract
		Then(order, Then(payment, delivery))
	}

	/* Example of contract usage: */
	val aBuyer		= Agent("Hugo")
	val aSeller		= Agent("Jonas")
	val anItem		= Item("Bike", 100)

	/* Instantiation of a contract using the above details */
	val buyBike = singleItemSale(aBuyer, aSeller, anItem, 100)

	/* Transactions */
	val anOrder		= Transaction(aBuyer, aSeller, anItem, Date())
	val aPayment	= Transaction(aBuyer, aSeller, MonetaryValue(anItem.value), Date())
	val aDelivery	= Transaction(aSeller, aBuyer, anItem, Date())

	val orderPlaced	  = evalC (anOrder) (buyBike)
	val orderPaid	  = evalC (aPayment) (orderPlaced)
	val itemDelivered = evalC (aDelivery) (orderPaid) // returns success
}