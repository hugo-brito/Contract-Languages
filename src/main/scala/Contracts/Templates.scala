package Contracts

import Language._
import Dates._

object Templates extends App {

	/* Simple contract for the exchange of resources. */
	def singleItemSale (buyer: Agent, seller: Agent, item: Item, amountPaid: Int): Contract = {
		// The transactions the template is expecting
		lazy val order	  = Atom(t => t.instigator == buyer && t.recipient == seller && t.resource == item)
		lazy val payment  = Atom(t => t.instigator == buyer && t.recipient == seller && t.resource == MonetaryValue(amountPaid))
		lazy val delivery = Atom(t => t.instigator == seller && t.recipient == buyer && t.resource == item)
		// The way the tranctions are combined in the given contract
		// Seq(order,Seq(payment,delivery))
		order followedBy payment followedBy delivery
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

	// The below templates does not work correctly, yet.
	def extra (att: Agent, com: Agent, invoice: Invoice) = {
        lazy val sendInvoice = Atom(t => t.instigator == att && t.recipient == com && t.resource == invoice)
        lazy val payInvoice = Atom(t => t.instigator == com && t.recipient == att && t.resource == invoice.total && t.timeStamp < invoice.due)
        Succ && (sendInvoice followedBy payInvoice)
        // Encoding due date into invoice instead of when first event received, since I am not sure how to get the
        // info from that.
        // how to get time from one Atom if want to use that in other?
        // page 500, fig 14.
    }

    def legal (att: Agent, com: Agent, invoice: Invoice, fee: MonetaryValue, start: Date, end: Date) :Contract = {
        lazy val attService = Atom(t => t.instigator == att && t.recipient == com && start < t.timeStamp && t.timeStamp < start + Months(1))
        lazy val extraWork = extra (att, com, invoice)
        lazy val payService = Atom(t => t.instigator == com && t.recipient == att && t.resource == fee && t.timeStamp < start + Days(8))
        lazy val nextSer = legal(att, com, invoice, fee, min(start + Months(1), end), end)
        lazy val finish = Atom(t => t.timeStamp == end && t.timeStamp > end)
        // if (start = end || start > end)
        attService followedBy (extraWork || payService || (nextSer || finish))
    }


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

}