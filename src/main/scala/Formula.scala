abstract class Formula {
  def &&(that: Formula) = And(this, that)
  def ||(that: Formula) = Or(this, that)
  def ==>(that: Formula) = Imply(this, that)
  def unary_! = Neg(this)

  def expression: String

  /** Push ￢ into innermost */
  def toNNF(implicit negative: Boolean = false): Formula
}

abstract class LogicalConnectives extends Formula

object False extends Formula {
  override def expression: String = "False"

  override def toNNF(implicit negative: Boolean = false) =
    if (negative) True else this
}
object True extends Formula {
  override def expression: String = "True"

  override def toNNF(implicit negative: Boolean = false) =
    if (negative) False else this
}

case class Bool(name: String) extends Formula {
  override def expression: String = name

  override def toNNF(implicit negative: Boolean = false) =
    if (negative) Neg(this) else this
}
object Bool {
  var count: Int = 0
  def apply(prefix: Option[String] = None): Bool = synchronized {
    count += 1
    new Bool(prefix.getOrElse("") + count.toString)
  }
  def resetCount(): Unit = { count = 0 }
}

case class And(f1: Formula, f2: Formula) extends LogicalConnectives {
  override def expression: String =
    "( " + f1.expression + " ∧ " + f2.expression + " )"

  override def toNNF(implicit negative: Boolean = false) =
    if (negative)
      Or(Neg(f1), Neg(f2)).toNNF(false)
    else
      (f1.toNNF(negative), f2.toNNF(negative)) match {
        case (_, False) | (False, _) => False
        case (f1, True)              => f1
        case (True, f2)              => f2
        case (f1, f2)                => And(f1, f2)
      }
}
case class Or(f1: Formula, f2: Formula) extends LogicalConnectives {
  override def expression: String =
    "( " + f1.expression + " ∨ " + f2.expression + " )"

  override def toNNF(implicit negative: Boolean = false) =
    if (negative)
      And(Neg(f1), Neg(f2)).toNNF(false)
    else
      (f1.toNNF(negative), f2.toNNF(negative)) match {
        case (f1, False)           => f1
        case (False, f2)           => f2
        case (_, True) | (True, _) => True
        case (f1, f2)              => Or(f1, f2)
      }
}
case class Imply(f1: Formula, f2: Formula) extends LogicalConnectives {
  override def expression: String =
    "( " + f1.expression + " ⊃ " + f2.expression + " )"

  override def toNNF(implicit negative: Boolean = false) =
    Or(Neg(f1), f2).toNNF(negative)
}
case class Neg(f1: Formula) extends LogicalConnectives {
  override def expression: String = "¬( " + f1.expression + " )"

  override def toNNF(implicit negative: Boolean = false) = f1.toNNF(!negative)
}

case class Literal(p: Bool, negative: Boolean) {
  def complement = Literal(p, !negative)

  def toDisplay: String = negative match {
    case true  => "-" + p.name
    case false => p.name
  }
}

case class Clause(literals: Set[Literal] = Set()) {
  def isEmpty = literals.isEmpty
  def tautology = literals.exists(lit => literals.contains(lit.complement))
  def contains(lit: Literal) = literals.contains(lit)
  def +(lit: Literal) = Clause(literals + lit)

  def toCnfExpression: String =
    "( " + literals.map(_.toDisplay).mkString(" ∨ ") + " )"
}
