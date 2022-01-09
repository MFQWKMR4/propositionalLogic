import Util._

object Sudoku {

  val BOX = "box"
  val ROW = "row"
  val COL = "col"
  val numbers: Seq[Int] = 0 to 8

  def solve(input: Array[String]): CnfOutput = {
    // prepare variables
    Bool.resetCount()
    val variables = Array.fill(9, 9, 9)(Bool())

    val condition = input
      .map {
        case "１" => 1
        case "２" => 2
        case "３" => 3
        case "４" => 4
        case "５" => 5
        case "６" => 6
        case "７" => 7
        case "８" => 8
        case "９" => 9
        case _   => 0
      }
      .zipWithIndex
      .map {
        case (value, i) =>
          val idx = i - 1
          val row = idx / 9
          val col = idx % 9
          value match {
            case 0 => Clause()
            case n =>
              Clause(Set(Literal(variables(row)(col)(n - 1), negative = false)))
          }
      }
      .filter { c =>
        0 < c.literals.size
      }
      .toSet

    def eachContainsPreciselyOneNumber(i: Int,
                                       j: Int,
                                       kind: String): Set[Clause] = {

      numbers.flatMap { self =>
        numbers
          .filterNot(_ == self)
          .map {
            // self ならば other でない
            other =>
              kind match {
                case BOX => !variables(i)(j)(self) || !variables(i)(j)(other)
                case COL => !variables(i)(self)(j) || !variables(i)(other)(j)
                case ROW => !variables(self)(i)(j) || !variables(other)(i)(j)
              }
          }
          .map(flattenOr)
          .map(toClause)
      }.toSet
    }

    //  Each box contains precisely one number.
    val c1 = for {
      i <- 0 to 8
      j <- 0 to 8
    } yield {
      val cond = toClause(numbers.map(k => variables(i)(j)(k)).toSet)
      eachContainsPreciselyOneNumber(i, j, BOX) + cond
    }

    //  Precisely once in each row (fix i ):
    val c2 = for {
      i <- 0 to 8
      k <- 0 to 8
    } yield {
      val cond = toClause(numbers.map(j => variables(i)(j)(k)).toSet)
      eachContainsPreciselyOneNumber(i, k, ROW) + cond
    }

    //  Precisely once in each colum (fix j):
    val c3 = for {
      j <- 0 to 8
      k <- 0 to 8
    } yield {
      val cond = toClause(numbers.map(i => variables(i)(j)(k)).toSet)
      eachContainsPreciselyOneNumber(j, k, COL) + cond
    }

    //  Precisely once in each sub-3x3 matrix.
    val c4 = numbers.flatMap { k =>
      for {
        col <- Seq(0, 3, 6)
        row <- Seq(0, 3, 6)
      } yield {
        val indexMap = (for {
          mov1 <- 0 to 2
          mov2 <- 0 to 2
        } yield (col + mov1, row + mov2)).zipWithIndex.map {
          case (indexes, i) => i -> indexes
        }.toMap

        val cond = toClause(numbers.map { n =>
          val (i, j) = indexMap(n)
          variables(i)(j)(k)
        }.toSet)

        val clauseSet = numbers.flatMap { self =>
          numbers
            .filterNot(_ == self)
            .map {
              // self ならば other でない
              other =>
                val selfIndex = indexMap(self)
                val otherIndex = indexMap(other)

                !variables(selfIndex._1)(selfIndex._2)(k) ||
                !variables(otherIndex._1)(otherIndex._2)(k)
            }
            .map(flattenOr)
            .map(toClause)
        }.toSet

        clauseSet + cond
      }
    }

    toCnfOutput((c1 ++ c2 ++ c3 ++ c4).foldLeft(condition) {
      case (acc, c) => acc ++ c
    })
  }

  def display(): Unit = {
    val reader = getFileReader("miniSAT.txt")
    val header = reader.readLine()
    header match {
      case "UNSAT" => println("fail")
      case "SAT" => {
        val answer = reader.readLine().split(" ").filterNot(_ == "0")
        val output = (0 to 80).foldLeft("") {
          case (acc, i) =>
            val start = i * 9
            val k = answer
              .slice(start, start + 9)
              .zipWithIndex
              .filterNot { case (v, _) => v.startsWith("-") }
              .map { case (_, i) => i + 1 }
              .head
            if (i % 9 == 8) {
              acc + s" $k \n"
            } else {
              acc + s" $k"
            }
        }
        println(output)
      }
    }
  }
}
