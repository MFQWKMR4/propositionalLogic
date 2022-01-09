case class Sequent(leftFormulae: List[Formula], rightFormulae: List[Formula]) {

  def expression: String = {
    val l = leftFormulae.map(_.expression).mkString(", ")
    val r = rightFormulae.map(_.expression).mkString(", ")
    l + " ⇨ " + r
  }

  def leftHandHasLogicalConnectives: Boolean = {
    val (a, _) = leftFormulae.partition {
      case l: LogicalConnectives => true
      case _                     => false
    }
    a.nonEmpty
  }

  def rightHandHasLogicalConnectives: Boolean = {
    val (a, _) = rightFormulae.partition {
      case l: LogicalConnectives => true
      case _                     => false
    }
    a.nonEmpty
  }

}
case class Proof(endsequent: Sequent, rule: String, subproofs: List[Proof]) {

  private val horizonalDivideToken = "---"
  private val emptyLineToken = "___"

  def proofFigure(depth: Int): (Int, String) = {
    val pf = s"### depth=${depth} ### " + endsequent.expression + " ### " + rule + " ###"
    val tmp = subproofs.map(_.proofFigure(depth + 1))
    tmp.length match {
      case 0 => (pf.length, pf)
      case 1 =>
        (tmp.head._1.max(pf.length), tmp.head._2 + horizonalDivideToken + pf)
      case 2 => {
        val t1 = tmp.head._2
        val t2 = tmp.tail.head._2
        val sub = t1 + emptyLineToken + t2
        (pf.length, sub + horizonalDivideToken + pf)
      }
    }
  }

  def getProofFigure(): Unit = {
    val ret = proofFigure(0)._2
      .replaceAll(emptyLineToken, "\n")
      .replaceAll(horizonalDivideToken, "\n")
    println(
      "### proof figure ###############################################################################################################"
    )
    println(ret)
    println(
      "################################################################################################################################"
    )
    println()
  }
}

object LK {

  // format: off

  private def applyDecompositionRule(
    sequent: Sequent
  ): Option[(String, List[Sequent])] = {

    val l = sequent.leftFormulae
    val r = sequent.rightFormulae

    val (a1, a2) = l.span {
      case Bool(_) => true
      case _       => false
    }

    a2 match {
      case Nil =>
        val (b1, b2) = r.span {
          case Bool(_) => true
          case _       => false
        }
        b2 match {
          case Nil => None
          case h :: t =>
            h match {
              /* orR */
              case Or(f1, f2) => Some(("orR", List(Sequent(l, b1 ::: f1 :: f2 :: t))))
              /* negR */
              case Neg(f1) => Some(("negR", List(Sequent(a1 ::: f1 :: Nil, b1 ::: t))))
              /* impR */
              case Imply(f1, f2) => Some(("impR", List(Sequent(a1 ::: f1 :: Nil, b1 ::: f2 :: t))))
              /* andR */
              case And(f1, f2) => Some(("andR", List(Sequent(l, b1 ::: f1 :: t), Sequent(l, b1 ::: f2 :: t))))
            }
        }
      case h :: t =>
        h match {
          /* andL */
          case And(f1, f2) => Some(("andL", List(Sequent(a1 ::: f1 :: f2 :: t, r))))
          /* negL */
          case Neg(f1) => Some(("negL", List(Sequent(a1 ::: t, f1 :: r))))
          /* orL */
          case Or(f1, f2) => Some(("orL", List(Sequent(a1 ::: f1 :: t, r), Sequent(a1 ::: f2 :: t, r))))
          /* impL */
          case Imply(f1, f2) => Some(("impL", List(Sequent(a1 ::: t, f1 :: r), Sequent(a1 ::: f2 :: t, r))))
        }
    }
  }

  def proveAll(seqs: List[Sequent]): Option[List[Proof]] = {
    if (seqs.isEmpty)
      Some(Nil)
    else
      prove(seqs.head) match {
        case None => None
        case Some(proof) =>
          proveAll(seqs.tail) match {
            case None         => None
            case Some(proofs) => Some(proof :: proofs)
          }
      }
  }

  def prove(sequent: Sequent): Option[Proof] = {

    val l = sequent.leftHandHasLogicalConnectives
    val r = sequent.rightHandHasLogicalConnectives

    (l, r) match {
      /* no logical connectives */
      case (false, false) => {
        if (sequent.leftFormulae.exists(f => sequent.rightFormulae.contains(f))) {
          /* axiom */
          Some(Proof(sequent, "axiom", Nil))
        } else {
          /* unprovable */
          None
        }
      }
      case _ => {
        applyDecompositionRule(sequent) match {
          /* apply decomposition rule, get rule and new sequent */
          case Some((rule, seqs)) => {
            /* prove new sequent */
            proveAll(seqs) match {
              /* unprovable */
              case None =>
                None
              /* get some proofs */
              case Some(proofs) =>
                Some(Proof(sequent, rule, proofs))
            }
          }
          /* apply decomposition rule, but failed */
          case None => None
        }
      }
    }
  }
  // format: on
}
