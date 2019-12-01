package Contracts

import Language._
import Dates._

object Templates extends App {

	def itemSale(buyer: Agent, seller: Agent, item: Item, amountPaid: MonetaryValue) = {
		val ord = Commitment(t => if (t.ins == buyer && t.rec == seller && t.res == item) Some(Succ) else None)
		val pay_del = Commitment(t => if (t.ins == buyer && t.rec == seller && t.res == amountPaid) 
							Some(Commitment(td =>
								if (td.ins == seller && td.rec == buyer && td.res == item && td.time < t.time + Days(2))
								Some(Succ) else None
							))
							else None)
		ord seq pay_del
	}

	/**
	 * This is the example that have been gone through in the report.
	 */
	def salesContract (buyer: Agent, seller: Agent, goods: Resource, pay: Double) :Contract= {
		lazy val ord_del_pay = Commitment(t => if (t.ins == buyer && t.rec == seller && t.res == goods) //order
							Some(Commitment(t2 => // 1st payment
								if (t2.ins == buyer && t2.rec == seller && t2.res == MonetaryValue(pay/2) && t2.time == t.time)
									Some(Commitment(t3 => // delivery
										if(t3.ins == seller && t3.rec == buyer && t3.res == goods && t3.time <= (t.time + Days(7)))
											Some(Commitment(t4 => // 2nd payment
												if (t4.ins == buyer && t4.rec == seller && t4.res == MonetaryValue(pay/2) && t4.time <= t3.time + Days(30))
												Some(salesContract(buyer, seller, goods, pay) // Can order again
													or // if 2nd payment late
													Commitment(t5 => if (t5.ins == buyer && t5.rec == seller && t5.res == MonetaryValue(1.1*(pay/2)) && t5.time <= t3.time +Days(44))
													Some(salesContract(buyer, seller, goods, pay)) // can order again
													else None
													)
												)
												else None										
											))
										else None
									))
								else None
							))
							else None)
		ord_del_pay
	}
}