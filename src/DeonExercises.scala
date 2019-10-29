import java.time.LocalDate

object DeonExercises extends App {
    
    abstract class Event
        case class agent(a: String) extends Event
        case class timestamp(t: LocalDate) extends Event

    
    
    
    println("Hello")
}