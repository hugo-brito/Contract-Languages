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
    case class Transaction(instigator: Agent, recipient: Agent, resource: Resource, timeStamp: Date, isAnOrder: Boolean = false, isAReturn: Boolean = false)
    object Transaction {
        def Order(instigator: Agent, recipient: Agent, resource: Resource, timeStamp: Date): Transaction =
            new Transaction(instigator, recipient, resource: Resource, timeStamp: Date, true, false)
            // could be nice to enforce that orders could only be on items or shopping cart
        def Return(instigator: Agent, recipient: Agent, resource: Resource, timeStamp: Date): Transaction =
            new Transaction(instigator, recipient, resource: Resource, timeStamp: Date, false, true)
            // This way, both the buyer and the seller can return the items and the amount that refers to the purchase
    }

    // Contract type
    sealed trait Contract {
        def ||(that: Contract) = Or(this, that)
        def &&(that: Contract) = Union(this, that)
        def followedBy(that: Contract) = Seq(this, that)
    }
    case class Atom(val f: Transaction => Boolean) extends Contract
    case class Seq(val c1: Contract, val c2: Contract) extends Contract
    case class Or(val c1: Contract, val c2: Contract) extends Contract
    case class Union(val c1: Contract, val c2: Contract) extends Contract
    case object Succ extends Contract
    case object Fail extends Contract

    // Evaluator function
    def evalC (e: Transaction) (c: Contract) :Contract = { 

        def reduceOr (c1: Contract, c2: Contract): Contract = {
            val evalC1 = evalC (e) (c1)
            lazy val evalC2 = evalC (e) (c2)
            evalC1 match {
                case Fail => evalC2
                case _    => c1
            }
        }

        def reduceSeq (c1: Contract, c2: Contract): Contract = {
            val resc1 = evalC (e) (c1)
            resc1 match {
                case Fail => Fail
                case Succ => c2
                case _ => resc1 followedBy c2 //Seq(resc1,c2)
            }
        }
        
        def reduceUnion (c1: Contract,c2: Contract) : Contract = {
            val evalC1 = evalC (e) (c1)
            val evalC2 = evalC (e) (c2)
            (evalC1, evalC2) match {
                    case (Fail, Fail)   => Fail
                    case (Succ, Succ)   => Succ
                    case (Fail, _)      => c1 && evalC2
                    case (_, Fail)      => evalC1 && c2
                    case (_,_)          => evalC1 && evalC2
            }
        }

        c match {
            case Atom(f)        => if (f(e)) Succ else Fail
            case Or(c1,c2)      => reduceOr (c1,c2)
            case Seq(c1,c2)     => reduceSeq (c1,c2)
            case Union(c1,c2)   => reduceUnion (c1,c2)
            case Succ           => Succ
            case Fail           => Fail // contract template can specify when it should fail
        }
    }

    def prettyPrinting (c: Contract): Unit = c match {
        case Atom(f)        => print(f)
        case Or(c1,c2)      => print("OR: " + prettyPrinting(c1) + " is satisfied or " + prettyPrinting(c2) + " is satisfied\n")
        case Seq(a,c)      => print("Seq: First " + prettyPrinting(a) + " must be satistied and then " + prettyPrinting(c) + " must be satified\n")
        case Union(c1,c2)   => print("UNION: awaiting matching events in no particular order for " + prettyPrinting(c1) + " and " + prettyPrinting(c2) + "\n")
        case Succ           => println("SUCCESS")
        case Fail           => println("BREACHED CONTRACT")
    }
}