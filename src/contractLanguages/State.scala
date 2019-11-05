package contractLanguages

case class State()

// state keeps track of state of execution of a contract.
// a contract can also be thought of a timeline of events
// that could branch in different directions. so the idea
// with state is to keep track of what events have happened
// and what is yet to happen so that the contract is finished.
// either it has 
