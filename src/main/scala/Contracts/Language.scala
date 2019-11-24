package Contracts

import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;

object Language extends App {

    // Agent type
    case class Agent(name: String)

    sealed trait Resource
    case class Item(name: String, value: Int) extends Resource
    case class MonetaryValue(amount: Int) extends Resource
    case class ShoppingCart(items: List[Item]) extends Resource

    case class Date (date: LocalDateTime){
        def diff (that :Date, unit: String) = 
            this.date.until(that.date, ChronoUnit.valueOf(unit)) // unit should be capital

        def -(that: Date) = that.date.until(this.date, ChronoUnit.valueOf("DAYS"))
        
        def +(days: Long) = this.date plus(days, ChronoUnit.valueOf("DAYS"))
    }
    object Date {
        def apply(year: Int, month: Int, dayOfMonth: Int, hour: Int, minutes: Int): Date =
            new Date(LocalDateTime.of(year, month, dayOfMonth, hour, minutes))
        
        def apply(): Date = 
            new Date(LocalDateTime.now())
    }
    
    // Event type
    case class Transaction(a1: Agent, a2: Agent, resource: Resource, timeStamp: Date)

    // Contract type
    sealed trait Contract {
        def ||(that: Contract) = Or(this, that)
        def +(that: Contract) = Union(this, that)
        def next(that: Contract) = Seq(this, that)
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
                case _ => c1
            }
        }

        def reduceSeq (c1: Contract, c2: Contract): Contract = {
            val resc1 = evalC (e) (c1)
            resc1 match {
                case Fail => Fail
                case Succ => c2
                case _ => resc1 next c2
            }
        }
        
        def reduceUnion (c1: Contract,c2: Contract) : Contract = {
            val evalC1 = evalC (e) (c1)
            val evalC2 = evalC (e) (c2)
            (evalC1, evalC2) match {
                    case (Fail, Fail)   => Fail
                    case (Succ, Succ)   => Succ
                    case (Fail, _)      => c1 + evalC2
                    case (_, Fail)      => evalC1 + c2
                    case (_,_)          => evalC1 + evalC2
            }
        }

        c match {
            case Atom(f)        => if (f(e)) Succ else Fail
            case Or(c1,c2)      => reduceOr (c1,c2)
            case Seq(c1,c2)     => reduceSeq (c1,c2)
            case Union(c1,c2)   => reduceUnion (c1,c2)
            case Succ           => Succ
            case Fail           => Fail
        }
    }
}