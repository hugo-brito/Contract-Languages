package Contracts

import Language._
import Dates._

object Templates extends App {

	val buyer = Agent("Jonas")
    val seller = Agent("Hugo")
    val item = Item("bike", 3499)
    val amountPaid = 3499
    val t1 = Transaction(buyer,seller, item, Date())
    val t2 = Transaction(buyer,seller, MonetaryValue(amountPaid), Date())
    val t3 = Transaction(seller,buyer, item, Date())

	def order(buyer: Agent, seller: Agent, item: Item) = {
		println(buyer + "," + seller + "," + item)
		Commitment(t => {
			if (t.instigator == buyer && t.recipient == seller && t.resource == item) {
				println("Order event evaluated Succ")
				Some(Succ)
			} else {
				println(s"Order event not succesful ${t}")
				None
			}
		})
	}

	def payment(buyer: Agent, seller: Agent, amount: Int) = Commitment(t => {
		println(s"${buyer},${seller},${MonetaryValue(amount)}")
		if (t.instigator == buyer && t.recipient == seller && t.resource == MonetaryValue(amount)) {
			println("Order event evaluated Succ")
			Some(Succ)
		} else {
			println(s"${t}")
			None
		}
	})

	def delivery(buyer: Agent, seller: Agent, item: Item) = Commitment(t => {
		if (t.instigator == seller && t.recipient == buyer && t.resource == item) Some(Succ) else None
	})

	val ord = order(buyer, seller, item)
	val pay = payment(buyer, seller, amountPaid)
	val del = delivery(buyer, seller, item)
	val suc = Succ then ord
	val first =  (ord or pay) then del
	val sec = (ord then del) or (pay then del)

	evalC (t1) (suc) // should be ord
	val firstEval = evalC (t1) (first) // should be del
	val secEval = evalC (t1) (sec) // should be sec
	evalC (t3) (firstEval) // should be Succ
	evalC (t3) (secEval) // should be Succ

	/* Simple contract for the exchange of resources. */
	def singleItemSale (buyer: Agent, seller: Agent, item: Item, amountPaid: Int): Contract = {
		// The transactions the template is expecting
		// lazy val order	  = Atom(t => t.instigator == buyer && t.recipient == seller && t.resource == item)
		lazy val order = Commitment(t => if (t.instigator == buyer && t.recipient == seller && t.resource == item) Some(Succ) else None)
		// lazy val payment  = Atom(t => t.instigator == buyer && t.recipient == seller && t.resource == MonetaryValue(amountPaid))
		lazy val payment = Commitment(t => if (t.instigator == buyer && t.recipient == seller && t.resource == MonetaryValue(amountPaid)) Some(Succ) else None)
		// lazy val delivery = Atom(t => t.instigator == seller && t.recipient == buyer && t.resource == item)
		lazy val delivery = Commitment(t => if (t.instigator == seller && t.recipient == buyer && t.resource == item) Some(Succ) else None)
		// The way the tranctions are combined in the given contract
		order then (payment then delivery)
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
	// def extra (att: Agent, com: Agent, invoice: Invoice) = {
    //     lazy val sendInvoice = Atom(t => t.instigator == att && t.recipient == com && t.resource == invoice)
    //     lazy val payInvoice = Atom(t => t.instigator == com && t.recipient == att && t.resource == invoice.total && t.timeStamp < invoice.due)
    //     Succ && (sendInvoice then payInvoice)
    //     // Encoding due date into invoice instead of when first event received, since I am not sure how to get the
    //     // info from that.
    //     // how to get time from one Atom if want to use that in other?
    //     // page 500, fig 14.
    // }

    // def legal (att: Agent, com: Agent, invoice: Invoice, fee: MonetaryValue, start: Date, end: Date) :Contract = {
    //     lazy val attService = Atom(t => t.instigator == att && t.recipient == com && start < t.timeStamp && t.timeStamp < start + Months(1))
    //     lazy val extraWork = extra (att, com, invoice)
    //     lazy val payService = Atom(t => t.instigator == com && t.recipient == att && t.resource == fee && t.timeStamp < start + Days(8))
    //     lazy val nextSer = legal(att, com, invoice, fee, min(start + Months(1), end), end)
    //     lazy val finish = Atom(t => t.timeStamp == end && t.timeStamp > end)
    //     // if (start = end || start > end)
    //     attService then (extraWork || payService || (nextSer || finish))
    // }


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