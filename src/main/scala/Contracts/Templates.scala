
// package Contracts

// import java.time.LocalDateTime;
// import Language._

// object Templates extends App {

//     def total(sb: ShoppingCart) : Int = {
//         def rHelper (l: List[Item]) (acc: Int) = 
//             l match {
//                 case Nill => acc
//                 case x::xs => rHelper (xs) (acc + x.value)
//             }
//         rHelper (sb.items) (0)
//     }

//     def multipleShop (buyer: Agent, seller: Agent, res: ShoppingCart) {
//         // val = Atom()
//     }

//     val jonas = Agent("Jonas")
//     val hugo = Agent("Hugo")

// //     // val orderBike = Order(10, "bike", jonas, hugo, "today")
// //     // val payBike = Payment(10, hugo, jonas, "today")
// //     // val deliverBiker = Delivery("bike", jonas, hugo, "today")

// //     // val buyBike = orderPayDelivery (hugo) (jonas) (10) ("bike")

// //     // evalC (orderBike) (buyBike)
// }