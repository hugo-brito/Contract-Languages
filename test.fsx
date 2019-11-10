open System

type Agent = {
    name: string
}

type Order = {
    amount: int
    item: string
    recipient: Agent // receives the event
    instigator: Agent // instigates out the the event
    time: string
}

type Delivery = {
    item: string
    recipient: Agent // receives the event
    instigator: Agent // instigates out the the event
    time: string
}

type Events = 
    | Ord of Order
    | Del of Delivery

type Contract = 
    | Atom of (Events -> bool)
    | Then of Contract * Contract
    | Or of Contract * Contract
    | Succ | Fail

let succCheck = function
    | Succ -> true
    | _ -> false

let rec evalC (e :Events) = function
    | Atom(f) -> if (f e) then Succ else Fail
    | Then(c1,c2) -> if (succCheck(evalC e c1)) then c2 else Fail
    | Or(c1,_) when succCheck (evalC e c1) -> Succ
    | Or(_,c2) when succCheck (evalC e c2) -> Succ
    | Or(_) -> Fail
    | Succ | Fail -> Fail // should not be in language

let date = "01/11/2019"
let item = "bike"
let seller = {name = "John"}
let buyer = {name = "Yvonne"}
let amount = 200

// write example contracts, use the eval function.

let templateResourceContract buyer seller amount item time : Contract = 
    let orderEvent = Atom (Ord {amount = amount;
                                    item = item;
                                    recipient = seller;
                                    instigator = buyer;
                                    time = time})

    let delEvent = Atom (Del {item = item;
                                    recipient = buyer;
                                    instigator = seller;
                                    time = time})
    Then(orderEvent,delEvent)

templateResourceContract buyer seller amount item date

/// There needs to happen an order event of hammer with price 10 made by agent Yvonne (buyer)
///  which John will then received at 01/11/2019 (date).
/// If that does not happen, the order will fail.
/// If it is does happen then it will succeed and the contract is complete.
Then(
    Then(
        Or(
            Atom(Ord{amount=10; item="hammer"; recipient=seller; instigator=buyer;time=date}),
            Atom(Ord{amount=10; item!="hammer"; recipient!=seller; instigator=buyer;time=date})),
        Fail),
    Succ)

// let ole = {name = "Ole"}
// let order = {amount = 10; agent =  ole; item = "Ost"}

// printf "amount: %i person: %s item: %s \n" order.amount order.agent.name order.item
// printf "%A\n" order