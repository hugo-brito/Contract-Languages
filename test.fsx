open System

type Agent = {
    name: string
}

type Order = {
    amount: int
    item: string
    recipient: Agent // receives the event
    instigator: Agent // carries out the the event
    time: string
}

type Delivery = {
    item: string
    recipient: Agent // receives the event
    instigator: Agent // carries out the the event
    time: string
}

type Events = 
    | Ord of Order
    | Del of Delivery

type Contract = 
    | Atom of Events
    | Seq of Contract * Contract

let date = "01/11/2019"
let item = "bike"
let seller = {name = "John"}
let buyer = {name = "Yvonne"}
let amount = 200

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
    Seq(orderEvent,delEvent)

templateResourceContract buyer seller amount item date

// let ole = {name = "Ole"}
// let order = {amount = 10; agent =  ole; item = "Ost"}

// printf "amount: %i person: %s item: %s \n" order.amount order.agent.name order.item
// printf "%A\n" order