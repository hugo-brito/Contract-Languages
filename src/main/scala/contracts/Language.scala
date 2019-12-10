package Contracts


import Dates._

object Language extends App {

    // Agent type
    case class Agent(name: String)

    sealed trait Resource
    case class Item(name: String) extends Resource
    case class MonetaryValue(amount: Double) extends Resource
    
    // Event type
    case class Transaction(ins: Agent, rec: Agent, res: Resource, time: Date)

    // Contract type
    sealed trait Contract {
        def or(that: Contract) = Or(this, that)
        def seq(that: Contract) :Contract= 
            this match {
                case Commitment(f) => Commitment(t => f(t) match {
                    case None => None
                    case Some(c1) => Some(c1 seq that)})
                case Or(c1,c2) => Or(c1 seq that, c2 seq that)
                case Succ => that
                case Fail => Fail
            }
    }

    case class Commitment(val f: Transaction => Option[Contract]) extends Contract
    case class Or(val c1: Contract, val c2: Contract) extends Contract
    case object Succ extends Contract
    case object Fail extends Contract

    // Return Option[Contract]
    // Evaluator function
    def reduce (e: Transaction) (c: Contract) :Contract = { 

        // orElse
        def reduceOr (c1: Contract, c2: Contract): Contract = {
            val c1p = reduce (e) (c1)
            lazy val c2p = reduce (e) (c2)
            c1p match {
                case Fail => c2p
                case _    => c1p
            }
        }

        def reduceCommitment (f: Transaction => Option[Contract], e: Transaction) = {
            val o = f(e)
            if (o.isDefined) o.get else Fail
        }

        c match {
            case Commitment(f)      => reduceCommitment (f, e)
            case Or(c1,c2)          => reduceOr (c1,c2)
            case Succ | Fail        => Fail
        }
    }
}