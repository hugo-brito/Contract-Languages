package contractLanguages

sealed trait Operator{}

sealed class True extends Operator{}
// only holds the value True

sealed class False extends Operator{}
// only holds the value False

sealed class Not extends Operator{}
// takes an expression a returns the opposite logical value

sealed class And extends Operator{}
// takes 2 expressions, if both are true, returns true
// else return false

sealed class Or extends Operator{}
// takes 2 expression, if one of the is true, returns true
// else return true

sealed class Then extends Operator{}
// if then else operator