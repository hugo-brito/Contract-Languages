package contracts

object Resource extends App {

    // Agent type
    case class Agent(name: String){
        def apply(name: String): Agent = new Agent(name)
        def printName: Unit = println(this.name)
    }

    // Event type
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

    // Contract type
    sealed trait Contract
    case class Atom(val f: Event => Boolean) extends Contract 
    case class Then(val c1: Contract, val c2: Contract) extends Contract 
    case class Or(val c1: Contract, val c2: Contract) extends Contract 
    case object Succ extends Contract {
        def succCheck (c: Contract): Boolean = c match {
            case Succ => true
            case _ 	  => false
        }
    }
    case object Fail extends Contract

    // Evaluator function
    def evalC (e: Event) (c: Contract) :Contract = c match {
        case Atom(f)        => if (f(e)) Succ else Fail
        case Then(c1,c2)    => {
            val evalC1 = evalC (e) (c1)
            val evalC2 = evalC (e) (c2)
            evalC1 match {
                case Fail => Fail
                case _    => 
            }
            // missing implementation
        case Or(c1,c2)      => {
            val evalC1 = evalC (e) (c1)
            evalC1 match {
                case Fail => evalC (e) (c2) // check if the left hand side doesn't fail, if it doesn't proceed with the right-hand side
                case _    => evalC1
            }
        case And(c1,c2)     => {
            val evalC1 = evalC (e) (c1)
            val evalC2 = evalC (e) (c2)
            evalC1 match {
                case Fail => Fail
                case _    => evalC2 match {
                    case Fail => Fail
                    case _    => // need to combine them 
                }
            }
        }
        case Succ | Fail    => Fail
    }

    // The remainder is to be able to make test Contract and see whether
    // it works as expected.
    def checkOrd (am: Int) (reci: Agent) (item: String) (e: Event): Boolean =
        e match {
            case Order(a,i,r,_,_)   => a == am && r == reci && i == item
            case _                  => false
        }

    def checkDel (item: String) (buyer: Agent) (e: Event): Boolean = 
        e match {
            case Delivery(i, r,_,_)   => i == item && r == buyer
            case _                    => false 
        }

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

    val jonas = Agent("Jonas")
    val hugo = Agent("Hugo")

    val orderBike = Order(10, "bike", jonas, hugo, "today")
    val payBike = Payment(10, hugo, jonas, "today")
    val deliverBiker = Delivery("bike", jonas, hugo, "today")

    val buyBike = orderPayDelivery (hugo) (jonas) (10) ("bike")

    evalC (orderBike) (buyBike)
}
