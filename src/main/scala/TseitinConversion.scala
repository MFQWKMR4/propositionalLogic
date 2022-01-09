import Util.{flattenAnd, flattenOr}

object TseitinConversion {

  def toCnfExpression(cnf: Set[_ <: Clause]): String =
    "( " + cnf.map(_.toCnfExpression).mkString(" ∧ ") + " )"

  def exec(formula: Formula): Set[Clause] = {
    // ANDで結合されているFormulaをキーにしてfresh constantを割り当てる
    var map: Map[Set[Formula], Bool] = Map()

    def transformNNF(f: Formula): Set[Clause] = {
      // toplevel and => Clauseの集合 に変換
      flattenAnd(f).map { g =>
        // toplevel or => literalの集合 に変換
        val literals = flattenOr(g).map {
          case p: Bool      => Literal(p, false)
          case Neg(p: Bool) => Literal(p, true)
          /* ANDが内部に含まれる場合 */
          case f: And => {
            val conj = flattenAnd(f)
            if (!map.contains(conj)) {
              val p = Bool()
              map += conj -> p
            }
            // fresh constant で置き換える
            Literal(map(conj), false)
          }
        }
        Clause(literals)
      }
    }

    /* Push ￢ into innermost */
    formula.toNNF match {
      case False => Set(Clause())
      case True  => Set()
      case nnf => {
        var cnf = transformNNF(nnf)
        while (map.nonEmpty) {
          // 置き換えていたANDのFormulaを取り出し
          val (fs, p) = map.head
          map -= fs
          // (p ならば f)　を cnf に追加する
          cnf |= fs.flatMap(f => transformNNF(Or(Neg(p), f)))
        }
        cnf
      }
    }
  }
}
