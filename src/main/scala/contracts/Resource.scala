package contracts

object Resource extends App {

    case class Agent(name: String){
        def apply(name: String): Agent = new Agent(name)
        def printName: Unit = println(this.name)
    }

    sealed trait Event
    case class Order(amount: Int, item: String, recipient: Agent, instigator: Agent, timeStamp: String) extends Event{
        def apply(am: Int, it: String, re: Agent, in: Agent, tS: String) = new Order(am, it, re, in, tS)
    }
    case class Delivery(item: String, recipient: Agent, instigator: Agent, timeStamp: String) extends Event{
        def apply(it: String, re: Agent, in: Agent, tS: String) = new Delivery(it, re, in, tS)
    }
    case class Payment(amount: Int, recipient: Agent, payer: Agent, timeStamp: String) extends Event{
        def apply(am: Int, re: Agent, pa: Agent, tS: String) = new Payment(am, re, pa, tS)
    }

    // type Contract = 
    //     | Atom of (Events -> bool)
    //     | Then of Contract * Contract
    //     | Or of Contract * Contract
    //     | Succ | Fail

    sealed trait Contract
    case class Atom(val f: Event => Boolean) extends Contract 
    case class Then(val c1: Contract, val c2: Contract) extends Contract 
    case class Or(val c1: Contract, val c2: Contract) extends Contract 
    case object Succ extends Contract {
        def succCheck (c: Contract): Boolean = c match {
            case Succ => true
            case _ 	 => false
        }
    }
    case object Fail extends Contract

    // // related to evaluation of contract
    // let succCheck = function
    //     | Succ -> true
    //     | _ -> false

    // let rec evalC (e :Events) = function
    //     | Atom(f) -> if (f e) then Succ else Fail
    //     | Then(c1,c2) -> if (succCheck(evalC e c1)) then c2 else Fail
    //     | Or(c1,_) when succCheck (evalC e c1) -> Succ
    //     | Or(_,c2) when succCheck (evalC e c2) -> Succ
    //     | Or(_) -> Fail
    //     | Succ | Fail -> Fail // should not be in language

    def evalC (e: Event) (c: Contract) :Contract = c match {
        case Atom(f)        => if (f(e)) Succ else Fail
        case Then(c1,c2)    => if (Succ.succCheck (evalC (e) (c1))) c2 else Fail
        case Or(c1,_) if (Succ.succCheck (evalC (e) (c1))) => Succ
        case Or(_,c2) if (Succ.succCheck (evalC (e) (c2))) => Succ
        case Or(_,_)        => Fail
        case Succ | Fail    => Fail
    }
 
    // // Related to contract template
    // let checkOrd (am :int) (reci :Agent) (item :string)  = function
    //     | Ord(o) -> o.amount = am && o.recipient = reci && o.item = item
    //     | _ -> false

    def checkOrd (am: Int) (reci: Agent) (item: String) (e: Event): Boolean =
        e match {
            case Order(a,i,r,_,_)   => a == am && r == reci && i == item
            case _                  => false
        }

    // let checkDel item buyer = function
    //     | Del(d) -> d.item = item && d.recipient = buyer
    //     | _ -> false

    def checkDel (item: String) (buyer: Agent) (e: Event): Boolean = 
        e match {
            case Delivery(i, r,_,_)   => i == item && r == buyer
            case _                    => false 
        }

    // let checkPay total reci = function
    //     | Pay(p) -> p.amount = total && p.recipient = reci
    //     | _ -> false

    def checkPay (total: Int) (seller :Agent) (e :Event): Boolean = 
        e match {
            case Payment(a, r,_,_)    => a == total && r == seller
            case _                    => false 
        }

    def orderPayDelivery (buyer: Agent) (seller: Agent) (amount: Int) (item: String): Contract = {
        val orderEvent = Atom(e => checkOrd (amount) (seller) (item) (e))
        val payEvent = Atom(e => checkPay (amount) (buyer) (e))
        val delEvent = Atom(e => checkDel (item) (buyer) (e))
        Then(orderEvent, Then(payEvent,delEvent))
    }

    // to create agents just use (fx):
    val jonas = Agent("Jonas")
    jonas.printName
    println(jonas)
    val hugo = Agent("Hugo")
    hugo == jonas


    val orderBike = Order(10, "bike", jonas, hugo, "today")
    val payBike = Payment(10, hugo, jonas, "today")
    val deliverBiker = Delivery("bike", jonas, hugo, "today")

    val buyBike = orderPayDelivery (hugo) (jonas) (10) ("bike")

    evalC (orderBike) (buyBike)
}