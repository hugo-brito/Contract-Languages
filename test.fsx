module ContractLanguage

type Agent = {
    name: string
}

type Order = {
    amount: int
    item: string
    recipient: Agent // receives the event
    instigator: Agent // instigates the event
    time: string
}

type Delivery = {
    item: string
    recipient: Agent // receives the event
    instigator: Agent // instigates the event
    time: string
}

type Payment = {
    amount: int
    payer: Agent
    recipient: Agent
    time: string
}

type Events = 
    | Ord of Order
    | Del of Delivery
    | Pay of Payment

type Contract = 
    | Atom of (Events -> bool)
    | Then of Contract * Contract
    | Or of Contract * Contract
    | Succ | Fail

// related to evaluation of contract
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


// Related to contract template
let checkOrd (am :int) (reci :Agent) (item :string) = function
    | Ord(o) -> o.amount = am && o.recipient = reci && o.item = item
    | _ -> false

let checkDel item buyer = function
    | Del(d) -> d.item = item && d.recipient = buyer
    | _ -> false

let checkPay total reci = function
    | Pay(p) -> p.amount = total && p.recipient = reci
    | _ -> false

let templateBuyContract buyer seller amount item : Contract =
    let orderEvent = Atom (fun e -> checkOrd amount seller item e)
    let payEvent = Atom (fun e -> checkPay amount seller e)
    let delEvent = Atom (fun e ->  checkDel item buyer e)
    Then(orderEvent, Then(payEvent, delEvent))

// to allow for easy creation of Contract
let initAgent name = {name=name}
let date = "01/11/2019"
let item = "bike"
let seller = initAgent "John"
let buyer = initAgent "Yvonne"
let amount = 200

let exampleContract = templateBuyContract buyer seller amount item

// To ease creation of events
let initOrder a i reci ins t = Ord({amount=a; item=i; recipient=reci; instigator=ins;time=t})
let initDel i reci ins t = Del({item=i;recipient=reci;instigator=ins;time=t})
let initPay a p reci t = Pay({amount=a;payer=p;recipient=reci;time=t})

let ordEvent = initOrder amount item seller buyer date
let wOrdEvent = initOrder 1 item seller buyer date
let payEvent = initPay amount buyer seller date
let wPayEvent = initPay amount buyer (initAgent "Daniel") date
let delEvent = initDel item buyer seller date
let wDelEvent = initDel "hammer" buyer seller date

let ordCon = evalC ordEvent exampleContract
let payCon = evalC payEvent ordCon
evalC delEvent payCon // evaluates the contract successfully

let wOrdCon = evalC wOrdEvent exampleContract
let wPayCon = evalC wPayEvent wOrdCon
let wdelCon = evalC wDelEvent payCon // all of these use incorrect events, hence it is evaluated incorrectly