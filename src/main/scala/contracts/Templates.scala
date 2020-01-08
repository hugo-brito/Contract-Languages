package Contracts

import Language._
import Dates._

// Contains all template contracts
object Templates extends App {

	// simple contract of a single item sale
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

	// Template for a sales contract like the one described in the report (Figure 3.1), where infinity is assumed.
	def salesContract (buyer: Agent, seller: Agent, goods: Resource, pay: Double) :Contract= {
		lazy val ord_del_pay = Commitment(t => if (t.ins == buyer && t.rec == seller && t.res == goods) //order
									Some(Commitment(t2 => // 1st payment
										if (t2.ins == buyer && t2.rec == seller && t2.res == MonetaryValue(pay/2) && t2.time == t.time)
											Some(Commitment(t3 => // delivery
												if(t3.ins == seller && t3.rec == buyer && t3.res == goods && t3.time < (t.time + Days(7)))
													Some(Commitment(t4 => // 2nd payment
														if (t4.ins == buyer && t4.rec == seller && t4.res == MonetaryValue(pay/2) && t4.time < t3.time + Days(30))
														Some(salesContract(buyer, seller, goods, pay) // Can order again
														)
														else None)
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
									else None)
		ord_del_pay
	}

	def rental_agreement (partyA: Agent, partyB: Agent, item: Item, days: Int, dailyFee: Double) :Contract= {
		val order = 
			Commitment(t =>	
				if (t.ins == partyA && t.rec == partyB && t.res == item)
					Some(Succ)
				else None)
		val payment = 
			Commitment(t1 => 
				if (t1.ins == partyA && t1.rec == partyB && t1.res == MonetaryValue(dailyFee * days))
					Some(Succ)
				else None)
		val receive_return = 
			Commitment(t1 	=> 	
				if (t1.ins == partyB && t1.rec == partyA && t1.res == item) { // exchange of camera
					val deadline = t1.time + Days(days)
					Some(Commitment(t2 => 
						if (t2.ins == partyA && t2.rec == partyB &&
							t2.res == item && t2.time <= deadline)
							Some(Succ)
						else None)
						or
						Commitment(t3 => 
							if (t3.ins == partyA && t3.rec == partyB &&
								t3.res == item && t3.time > t1.time + Days(days))
							Some(Commitment(t4 	=> {
								val daysPast = deadline diff (t4.time, "DAYS")
								if (t4.ins == partyA && t4.rec == partyB &&
									t4.res == MonetaryValue(daysPast*(days*1.5)))
									Some(Succ)
								else None}))
							else None))
				} else None)
		order seq payment seq receive_return
	}

	
}