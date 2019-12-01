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

    // Contract type
    sealed trait Contract {
        def or(that: Contract) = Or(this, that)
        def then(that: Contract) = seq(this, that)
    }
    case class Commitment(val f: Transaction => Option[Contract]) extends Contract
    case class Or(val c1: Contract, val c2: Contract) extends Contract
    case object Succ extends Contract
    case object Fail extends Contract

    def seq (c: Contract, c3: Contract) :Contract= 
        c match {
            case Commitment(f) => Commitment(t => f(t) match {
                case None => None
                case Some(c1) => Some(seq(c1,c3))
            })
            case Or(c1,c2) => Or(seq(c1,c3),seq(c2,c3))
            case Succ => c3
            case Fail => Fail
        }

    // Return Option[Contract]
    // Evaluator function
    def evalC (e: Transaction) (c: Contract) :Contract = { 

        // orElse
        def reduceOr (c1: Contract, c2: Contract): Contract = {
            val evalC1 = evalC (e) (c1)
            lazy val evalC2 = evalC (e) (c2)
            evalC1 match {
                case Fail => evalC2
                case _    => evalC1
            }
        }

        def isNullable (c: Contract): Boolean = 
            c match {
                case Succ       => true
                case Seq(c1,c2) => isNullable(c1) && isNullable(c2)
                case Or(c1,c2)  => isNullable(c1) || isNullable(c2)
                case _ => false
            }

        def reduceSeq (c1: Contract, c2: Contract): Contract = {
            lazy val c1p = evalC(e) (c1)
            lazy val c2p = evalC(e) (c2)
            lazy val c1_nuab = isNullable(c1)
            lazy val c1p_nuab = isNullable(c1p)
            lazy val c2p_nuab = isNullable(c2p)

            if (c1_nuab && !c1p_nuab){
                c2p
            } else if(!isNullable(c1)){
                c2p then c2
            } 
        }

        // If c1 is nullable and whether the event matches
        // f: (c1, c2) => c3
        // consider (Succ then Succ) then c1

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