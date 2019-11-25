package Contracts

import Language._
import Date._

object jTemplates extends App {

    def extra (att: Agent, com: Agent, invoice: Invoice) = {
        lazy val sendInvoice = Atom(t => t.a1 == att && t.a2 == com && t.resource == invoice)
        lazy val payInvoice = Atom(t => t.a1 == com && t.a2 == att && t.resource == invoice.total && t.timeStamp < invoice.due)
        Succ && (sendInvoice followedBy payInvoice)
        // Encoding due date into invoice instead of when first event received, since I am not sure how to get the
        // info from that.
        // how to get time from one Atom if want to use that in other?
        // page 500, fig 14.
    }

    def legal (att: Agent, com: Agent, invoice: Invoice, fee: MonetaryValue, start: Date, end: Date) :Contract = {
        lazy val attService = Atom(t => t.a1 == att && t.a2 == com && start < t.timeStamp && t.timeStamp < start + Months(1))
        lazy val extraWork = extra (att, com, invoice)
        lazy val payService = Atom(t => t.a1 == com && t.a2 == att && t.resource == fee && t.timeStamp < start + Days(8))
        lazy val nextSer = legal(att, com, invoice, fee, min(start + Months(1), end), end)
        lazy val finish = Atom(t => t.timeStamp == end && t.timeStamp > end)
        // if (start = end || start > end)
        attService followedBy (extraWork || payService || (nextSer || finish))
    }

    def singleResContract(buyer: Agent, seller: Agent, item: Item, payment: MonetaryValue) {
        lazy val order = Atom(t => t.a1 == buyer && t.a2 == seller && t.resource == item)
        lazy val pay = Atom(t => t.a2 == seller && t.a1 == buyer && t.resource == payment)
        lazy val delivery = Atom(t => t.a1 == buyer && t.a2 == seller && t.resource == item)
        order followedBy pay followedBy delivery
    }

    // PersonC buys an item from SellerB to be sent to personA aswell as items for
    // themselves now. PersonA is allowed to return items. 

    // def multiParty (buyer: Agent, seller: Agent, receiver: Agent, item1: Items, item2: Item, payment: MonetaryValue, time: Date) = {
    //     val order1 = Atom(t => t.a1 == buyer && t.a2 == seller && t.resource == item1)
    //     val order2 = Atom(t => t.a1 == buyer && t.a2 == seller && t.resource == item2)
    //     val pay = Atom(t => t.a1 == buyer && t.a2 == seller && t.resource >= payment)
    //     val del1 = Atom(t => t.a2 == seller && t.a1 == buyer && t.resource == item1)
    //     val del2 = Atom(t => t.a2 == seller && t.a2 == receiver && t.resource == item2 && t.timeStamp < time + 2)
    //     val ret = Atom(t => t.a1 == receiver && t.a2 == seller && t.resource == item2 && t.timeStamp <= deliveryDate + 14)
    //     Union(  
    //         order1, 
    //         Then(
    //             pay, 
    //             Union(
    //                 del1,
    //                 Then(
    //                     del2, 
    //                     Union(Succ, ret)))))
    // }
}