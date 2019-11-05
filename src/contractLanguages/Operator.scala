package contractLanguages

sealed trait Operator{}

sealed class And extends Operator{}

sealed class Or extends Operator{}

sealed class Then extends Operator{}