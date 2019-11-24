package Contracts

import Language._

object jTemplates extends App {

    def extra (att: Agent, com: Agent, invoice: Invoice, pay: MonetaryValue) = {
        val sendInvoice = Atom(t => t.a1 == att && t.a2 == com && t.resource == invoice)
        Succ + sendInvoice next Atom(f => true)
        // how to get time from one Atom if want to use that in other?
        // page 500, fig 14.
    }

    def legal (att: Agent, com: Agent, invoice: Invoice, pay: MonetaryValue, n: Days, m: Days, end: Days) = {
        // n is start of contract (month)
        // m is end of month (given n)
        // end is the end of the contract
    }


    def singleResContract(buyer: Agent, seller: Agent, item: Item, payment: MonetaryValue) {
        lazy val order = Atom(t => t.a1 == buyer && t.a2 == seller && t.resource == item)
        lazy val pay = Atom(t => t.a2 == seller && t.a1 == buyer && t.resource == payment)
        lazy val delivery = Atom(t => t.a1 == buyer && t.a2 == seller && t.resource == item)
        order next pay next delivery
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