package Contracts


import Dates._

object Language extends App {

    // Agent type
    case class Agent(name: String)

    sealed trait Resource
    case class Item(name: String, value: Int) extends Resource
    case class MonetaryValue(amount: Int) extends Resource
    case class ShoppingCart(items: List[Item]) extends Resource
    case class Invoice(seller: Agent, buyer: Agent, total: MonetaryValue, issue: Date, due: Date) extends Resource
    
    // Event type
    case class Transaction(instigator: Agent, recipient: Agent, resource: Resource, timeStamp: Date)

    /*
    , isAnOrder: Boolean = false, isAReturn: Boolean = false)
    object Transaction {
        def Order(instigator: Agent, recipient: Agent, resource: Resource, timeStamp: Date): Transaction =
            new Transaction(instigator, recipient, resource: Resource, timeStamp: Date, true, false)
            // could be nice to enforce that orders could only be on items or shopping cart
        def Return(instigator: Agent, recipient: Agent, resource: Resource, timeStamp: Date): Transaction =
            new Transaction(instigator, recipient, resource: Resource, timeStamp: Date, false, true)
            // This way, both the buyer and the seller can return the items and the amount that refers to the purchase
    }
    */


    // Contract type
    sealed trait Contract {
        def or(that: Contract) = Or(this, that)
        def then(that: Contract) = Seq(this, that)
    }
    case class Commitment(val f: Transaction => Option[Contract]) extends Contract
    case class Seq(val c1: Contract, val c2: Contract) extends Contract
    case class Or(val c1: Contract, val c2: Contract) extends Contract
    case object Succ extends Contract
    case object Fail extends Contract

    // Evaluator function
    def evalC (e: Transaction) (c: Contract) :Contract = { 

        def reduceOr (c1: Contract, c2: Contract): Contract = {
            val evalC1 = evalC (e) (c1)
            lazy val evalC2 = evalC (e) (c2)
            evalC1 match {
                case Fail => evalC2
                case _    => evalC1
            }
        }

        def reduceSeq (c1: Contract, c2: Contract): Contract = {
            val resc1 = evalC (e) (c1)
            (c1, resc1) match {
                case (Fail, _) => Fail
                case (Succ, _) => c2
                case (_, Fail) => Fail
                case (_, Succ) => c2
                case _ => resc1 then c2 //Seq(resc1,c2)
            }
        }

        def reduceCommitment (f: Transaction => Option[Contract], e: Transaction) = {
            val o = f(e)
            if (o.isDefined) o.get else Fail
        }

        c match {
            case Commitment(f)      => reduceCommitment (f, e)
            case Or(c1,c2)          => reduceOr (c1,c2)
            case Seq(c1,c2)         => reduceSeq (c1,c2)
            case Succ | Fail        => Fail
        }
    }

    def prettyPrinting (c: Contract): Unit = c match {
        case Commitment(f)    => print(c)
        case Or(c1,c2)      => print("OR: " + prettyPrinting(c1) + " is satisfied or " + prettyPrinting(c2) + " is satisfied\n")
        case Seq(a,c)       => print("Seq: First " + prettyPrinting(a) + " must be satistied and then " + prettyPrinting(c) + " must be satified\n")
        case Succ           => println("SUCCESS")
        case Fail           => println("BREACHED CONTRACT")
    }
}