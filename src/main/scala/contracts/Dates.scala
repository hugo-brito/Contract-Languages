package contracts

import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;

// Module for handling dates
object Dates extends App {

    // Different units of time.
    sealed trait Time {
        def unit :String
        def no :Long
    }
    case class Years(num: Long) extends Time {
        val units = "YEARS"
        def unit :String = this.units
        def no :Long = this.num
    }
    case class Months(num: Long) extends Time {
        val units = "MONTHS"
        def unit :String = this.units
        def no :Long = this.num
    }
    case class Weeks(num: Long) extends Time {
        val units = "WEEKS"
        def unit :String = this.units
        def no :Long = this.num
    }
    case class Days(num: Long) extends Time {
        val units = "DAYS"
        def unit :String = this.units
        def no :Long = this.num
    }
    case class Hours(num: Long) extends Time {
        val units = "HOURS"
        def unit :String = this.units
        def no :Long = this.num
    }
    case class Minutes(num: Long) extends Time {
        val units = "MINUTES"
        def unit :String = this.units
        def no :Long = this.num
    }

    // The date class with different prefixes to allow for comparison of Dates and calculation
    case class Date (date: LocalDateTime){
        def diff (that :Date, unit: String) = 
            this.date.until(that.date, ChronoUnit.valueOf(unit)) // units should be capital

        def >(that :Date) :Boolean = this.date isAfter that.date
        
        def <(that :Date) :Boolean = this.date isBefore that.date

        def <=(that :Date) :Boolean = this < that || this == that

        def ==(that :Date) = this.date equals that.date

        def -(that: Date) = that.date.until(this.date, ChronoUnit.valueOf("DAYS"))
        
        def -(time: Time) = Date(this.date minus(time.no, ChronoUnit.valueOf(time.unit)))

        def +(time: Time) = Date(this.date plus(time.no, ChronoUnit.valueOf(time.unit)))
    }

    // To allow for different instantiations of Dates
    object Date {
        def apply(year: Int, month: Int, dayOfMonth: Int, hour: Int, minutes: Int): Date =
            new Date(LocalDateTime.of(year, month, dayOfMonth, hour, minutes))
        
        def apply(year: Int, month: Int, dayOfMonth: Int): Date =
            new Date(LocalDateTime.of(year, month, dayOfMonth, 0, 0))

        def apply(): Date = 
            new Date(LocalDateTime.now())
    }

    // Get the smallest date out
    def min(d1: Date, d2: Date) :Date = 
    (d1,d2) match {
        case (d1,d2) if (d1 > d2) => d2
        case (d1,d2) if (d1 < d2) => d1
        case (d1,d2) if (d1 == d2) => d1
    }
}