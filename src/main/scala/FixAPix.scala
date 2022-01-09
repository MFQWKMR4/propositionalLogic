import Util._

object FixAPix {

  // format: off
  
  // reverse(true,false)
  def atMost1Of9(fs: Seq[Formula]) = {
    toAndFormula(for {
      a <- 0 to 7
      b <- (a+1) to 8
    } yield {
      fs(a) || fs(b)
    })
  }
  // reverse(true,false)
  def atMost2Of9(fs: Seq[Formula]) = {
    toAndFormula(for {
      a <- 0 to 6
      b <- (a+1) to 7
      c <- (b+1) to 8
    } yield {
      fs(a) || fs(b) || fs(c)
    })
  }
  // reverse(true,false)
  def atMost3Of9(fs: Seq[Formula]) = {
    toAndFormula(for {
      a <- 0 to 5
      b <- (a+1) to 6
      c <- (b+1) to 7
      d <- (c+1) to 8
    } yield {
      fs(a) || fs(b) || fs(c) || fs(d)
    })
  }
  // reverse(true,false)
  def atMost4Of9(fs: Seq[Formula]) = {
    toAndFormula(for {
      a <- 0 to 4
      b <- (a+1) to 5
      c <- (b+1) to 6
      d <- (c+1) to 7
      e <- (d+1) to 8
    } yield {
      fs(a) || fs(b) || fs(c) || fs(d) || fs(e)
    })
  }
  // reverse(true,false)
  def atMost1Of6(fs: Seq[Formula]) = {
    toAndFormula(for {
      a <- 0 to 4
      b <- (a+1) to 5
    } yield {
      fs(a) || fs(b)
    })
  }
  // reverse(true,false)
  def atMost2Of6(fs: Seq[Formula]) = {
    toAndFormula(for {
      a <- 0 to 3
      b <- (a+1) to 4
      c <- (b+1) to 5
    } yield {
      fs(a) || fs(b) || fs(c)
    })
  }
  // reverse(true,false)
  def atMost3Of6(fs: Seq[Formula]) = {
    toAndFormula(for {
      a <- 0 to 2
      b <- (a+1) to 3
      c <- (b+1) to 4
      d <- (c+1) to 5
    } yield {
      fs(a) || fs(b) || fs(c) || fs(d)
    })
  }
  // reverse(true,false)
  def atMost1Of4(fs: Seq[Formula]) = {
    toAndFormula(for {
      a <- 0 to 2
      b <- (a+1) to 3
    } yield {
      fs(a) || fs(b)
    })
  }
  // reverse(true,false)
  def atMost2Of4(fs: Seq[Formula]) = {
    toAndFormula(for {
      a <- 0 to 1
      b <- (a+1) to 2
      c <- (b+1) to 3
    } yield {
      fs(a) || fs(b) || fs(c)
    })
  }
  def atLeast1Of9(var1: Formula,
                  var2: Formula,
                  var3: Formula,
                  var4: Formula,
                  var5: Formula,
                  var6: Formula,
                  var7: Formula,
                  var8: Formula,
                  var9: Formula) = {
    (var1 || var2 || var3 || var4 || var5 || var6 || var7 || var8 || var9)
  }
  def atLeast4Of9(var1: Formula,
                  var2: Formula,
                  var3: Formula,
                  var4: Formula,
                  var5: Formula,
                  var6: Formula,
                  var7: Formula,
                  var8: Formula,
                  var9: Formula) = {
      atLeast3Of8(var1, var2, var3, var4, var5, var6, var7, var8) &&
      atLeast3Of8(var1, var2, var3, var4, var5, var6, var7, var9) &&
      atLeast3Of8(var1, var2, var3, var4, var5, var6, var8, var9) &&
      atLeast3Of8(var1, var2, var3, var4, var5, var7, var8, var9) &&
      atLeast3Of8(var1, var2, var3, var4, var6, var7, var8, var9) &&
      atLeast3Of8(var1, var2, var3, var5, var6, var7, var8, var9) &&
      atLeast3Of8(var1, var2, var4, var5, var6, var7, var8, var9) &&
      atLeast3Of8(var1, var3, var4, var5, var6, var7, var8, var9) &&
      atLeast3Of8(var2, var3, var4, var5, var6, var7, var8, var9)
  }
  def atLeast3Of8(var1: Formula,
                  var2: Formula,
                  var3: Formula,
                  var4: Formula,
                  var5: Formula,
                  var6: Formula,
                  var7: Formula,
                  var8: Formula) = {
    atLeast2Of7(var1, var2, var3, var4, var5, var6, var7) &&
    atLeast2Of7(var1, var2, var3, var4, var5, var6, var8) &&
    atLeast2Of7(var1, var2, var3, var4, var5, var7, var8) &&
    atLeast2Of7(var1, var2, var3, var4, var6, var7, var8) &&
    atLeast2Of7(var1, var2, var3, var5, var6, var7, var8) &&
    atLeast2Of7(var1, var2, var4, var5, var6, var7, var8) &&
    atLeast2Of7(var1, var3, var4, var5, var6, var7, var8) &&
    atLeast2Of7(var2, var3, var4, var5, var6, var7, var8)
  }
  def atLeast3Of9(var1: Formula,
                  var2: Formula,
                  var3: Formula,
                  var4: Formula,
                  var5: Formula,
                  var6: Formula,
                  var7: Formula,
                  var8: Formula,
                  var9: Formula) = {
    atLeast2Of8(var1, var2, var3, var4, var5, var6, var7, var8) &&
    atLeast2Of8(var1, var2, var3, var4, var5, var6, var7, var9) &&
    atLeast2Of8(var1, var2, var3, var4, var5, var6, var8, var9) &&
    atLeast2Of8(var1, var2, var3, var4, var5, var7, var8, var9) &&
    atLeast2Of8(var1, var2, var3, var4, var6, var7, var8, var9) &&
    atLeast2Of8(var1, var2, var3, var5, var6, var7, var8, var9) &&
    atLeast2Of8(var1, var2, var4, var5, var6, var7, var8, var9) &&
    atLeast2Of8(var1, var3, var4, var5, var6, var7, var8, var9) &&
    atLeast2Of8(var2, var3, var4, var5, var6, var7, var8, var9)
  }
  def atLeast2Of7(var1: Formula,
                  var2: Formula,
                  var3: Formula,
                  var4: Formula,
                  var5: Formula,
                  var6: Formula,
                  var7: Formula) = {
      (var1 || var2 || var3 || var4 || var5 || var6) &&
      (var1 || var2 || var3 || var4 || var5 || var7) &&
      (var1 || var2 || var3 || var4 || var6 || var7) &&
      (var1 || var2 || var3 || var5 || var6 || var7) &&
      (var1 || var2 || var4 || var5 || var6 || var7) &&
      (var1 || var3 || var4 || var5 || var6 || var7) &&
      (var2 || var3 || var4 || var5 || var6 || var7)
  }
  def atLeast2Of8(var1: Formula,
                  var2: Formula,
                  var3: Formula,
                  var4: Formula,
                  var5: Formula,
                  var6: Formula,
                  var7: Formula,
                  var8: Formula) = {
    (var1 || var2 || var3 || var4 || var5 || var6 || var7) &&
    (var1 || var2 || var3 || var4 || var5 || var6 || var8) &&
    (var1 || var2 || var3 || var4 || var5 || var7 || var8) &&
    (var1 || var2 || var3 || var4 || var6 || var7 || var8) &&
    (var1 || var2 || var3 || var5 || var6 || var7 || var8) &&
    (var1 || var2 || var4 || var5 || var6 || var7 || var8) &&
    (var1 || var3 || var4 || var5 || var6 || var7 || var8) &&
    (var2 || var3 || var4 || var5 || var6 || var7 || var8)
  }
  def atLeast2Of9(var1: Formula,
                  var2: Formula,
                  var3: Formula,
                  var4: Formula,
                  var5: Formula,
                  var6: Formula,
                  var7: Formula,
                  var8: Formula,
                  var9: Formula) = {
    (var1 || var2 || var3 || var4 || var5 || var6 || var7 || var8) &&
    (var1 || var2 || var3 || var4 || var5 || var6 || var7 || var9) &&
    (var1 || var2 || var3 || var4 || var5 || var6 || var8 || var9) &&
    (var1 || var2 || var3 || var4 || var5 || var7 || var8 || var9) &&
    (var1 || var2 || var3 || var4 || var6 || var7 || var8 || var9) &&
    (var1 || var2 || var3 || var5 || var6 || var7 || var8 || var9) &&
    (var1 || var2 || var4 || var5 || var6 || var7 || var8 || var9) &&
    (var1 || var3 || var4 || var5 || var6 || var7 || var8 || var9) &&
    (var2 || var3 || var4 || var5 || var6 || var7 || var8 || var9)
  }
  def atLeast1Of6(var1: Formula,
                  var2: Formula,
                  var3: Formula,
                  var4: Formula,
                  var5: Formula,
                  var6: Formula) = {
    (var1 || var2 || var3 || var4 || var5 || var6)
  }
  def atLeast2Of6(var1: Formula,
                  var2: Formula,
                  var3: Formula,
                  var4: Formula,
                  var5: Formula,
                  var6: Formula) = {
    (var1 || var2 || var3 || var4 || var5) &&
    (var1 || var2 || var3 || var4 || var6) &&
    (var1 || var2 || var3 || var5 || var6) &&
    (var1 || var2 || var4 || var5 || var6) &&
    (var1 || var3 || var4 || var5 || var6) &&
    (var2 || var3 || var4 || var5 || var6)
  }
  def atLeast1Of4(var1: Formula,
                  var2: Formula,
                  var3: Formula,
                  var4: Formula) = {
    (var1 || var2 || var3 || var4)
  }
  def atLeast2Of4(var1: Formula,
                  var2: Formula,
                  var3: Formula,
                  var4: Formula) = {
    (var2 || var3 || var4) &&
    (var2 || var3 || var1) &&
    (var2 || var1 || var4) &&
    (var1 || var3 || var4)
  }

  def solve_problem1_8050(): CnfOutput = {
    val reader = getFileReader("problem1_80x50.txt")
    val input = Iterator.fill(50)(reader.readLine().split(" ")).toArray

    // prepare variables
    Bool.resetCount()
    val vars = Array.fill(50, 80)(Bool())
    
    def preRow(i: Int, j: Int, negative: Boolean) = negative match {
      case true  => !vars(i - 1)(j - 1) && !vars(i - 1)(j) && !vars(i - 1)(j + 1)
      case false => vars(i - 1)(j - 1) && vars(i - 1)(j) && vars(i - 1)(j + 1)
    }
    def row(i: Int, j: Int, negative: Boolean) = negative match {
      case true  => !vars(i)(j - 1) && !vars(i)(j) && !vars(i)(j + 1)
      case false => vars(i)(j - 1) && vars(i)(j) && vars(i)(j + 1)
    }
    def postRow(i: Int, j: Int, negative: Boolean) = negative match {
      case true  => !vars(i + 1)(j - 1) && !vars(i + 1)(j) && !vars(i + 1)(j + 1)
      case false => vars(i + 1)(j - 1) && vars(i + 1)(j) && vars(i + 1)(j + 1)
    }

    // return CNF
    def choice9C1(var1: Formula, var2: Formula, var3: Formula, var4: Formula, var5: Formula, var6: Formula, var7: Formula, var8: Formula, var9: Formula, negative: Boolean): Formula = {
      negative match {
        case true => atLeast1Of9(!var1, !var2, !var3, !var4, !var5, !var6, !var7, !var8, !var9) && atMost1Of9(Seq(var1, var2, var3, var4, var5, var6, var7, var8, var9))
        case false => atLeast1Of9(var1, var2, var3, var4, var5, var6, var7, var8, var9) && atMost1Of9(Seq(!var1, !var2, !var3, !var4, !var5, !var6, !var7, !var8, !var9))
      }
    }
    // return CNF
    def choice9C2(var1: Formula, var2: Formula, var3: Formula, var4: Formula, var5: Formula, var6: Formula, var7: Formula, var8: Formula, var9: Formula, negative: Boolean): Formula = {
      negative match {
        case true => atLeast2Of9(!var1, !var2, !var3, !var4, !var5, !var6, !var7, !var8, !var9) && atMost2Of9(Seq(var1, var2, var3, var4, var5, var6, var7, var8, var9))
        case false => atLeast2Of9(var1, var2, var3, var4, var5, var6, var7, var8, var9) && atMost2Of9(Seq(!var1, !var2, !var3, !var4, !var5, !var6, !var7, !var8, !var9))
      }
    }
    // return CNF
    def choice9C3(var1: Formula, var2: Formula, var3: Formula, var4: Formula, var5: Formula, var6: Formula, var7: Formula, var8: Formula, var9: Formula, negative: Boolean): Formula = {
      negative match {
        case true => atMost3Of9(Seq(var1, var2, var3, var4, var5, var6, var7, var8, var9)) && atLeast3Of9(!var1, !var2, !var3, !var4, !var5, !var6, !var7, !var8, !var9)
        case false => atMost3Of9(Seq(!var1, !var2, !var3, !var4, !var5, !var6, !var7, !var8, !var9)) && atLeast3Of9(var1, var2, var3, var4, var5, var6, var7, var8, var9)
      }
    }
    // return CNF
    def choice9C4(var1: Formula, var2: Formula, var3: Formula, var4: Formula, var5: Formula, var6: Formula, var7: Formula, var8: Formula, var9: Formula, negative: Boolean): Formula = {
      negative match {
        case true => atMost4Of9(Seq(var1, var2, var3, var4, var5, var6, var7, var8, var9)) && atLeast4Of9(!var1, !var2, !var3, !var4, !var5, !var6, !var7, !var8, !var9)
        case false => atMost4Of9(Seq(!var1, !var2, !var3, !var4, !var5, !var6, !var7, !var8, !var9)) && atLeast4Of9(var1, var2, var3, var4, var5, var6, var7, var8, var9)
      }
    }
    // return CNF
    def choice6C1(var1: Formula, var2: Formula, var3: Formula, var4: Formula, var5: Formula, var6: Formula, negative: Boolean) = {
      negative match {
        case true => atLeast1Of6(!var1, !var2, !var3, !var4, !var5, !var6) && atMost1Of6(Seq(var1, var2, var3, var4, var5, var6))
        case false => atLeast1Of6(var1, var2, var3, var4, var5, var6) && atMost1Of6(Seq(!var1, !var2, !var3, !var4, !var5, !var6))
      }
    }
    // return CNF
    def choice6C2(var1: Formula, var2: Formula, var3: Formula, var4: Formula, var5: Formula, var6: Formula, negative: Boolean) = {
      negative match {
        case true => atLeast2Of6(!var1, !var2, !var3, !var4, !var5, !var6) && atMost2Of6(Seq(var1, var2, var3, var4, var5, var6))
        case false => atLeast2Of6(var1, var2, var3, var4, var5, var6) && atMost2Of6(Seq(!var1, !var2, !var3, !var4, !var5, !var6))
      }
    }
    // return CNF
    def choice6C3(base: Seq[Formula]) = atMost3Of6(base) && atMost3Of6(base.map(!_))
    // return CNF
    def choice4C1(var1: Formula, var2: Formula, var3: Formula, var4: Formula, negative: Boolean) = {
      negative match {
        case true => atLeast1Of4(!var1, !var2, !var3, !var4) && atMost1Of4(Seq(var1, var2, var3, var4))
        case false => atLeast1Of4(var1, var2, var3, var4) && atMost1Of4(Seq(!var1, !var2, !var3, !var4))
      }
    }
    // return CNF
    def choice4C2(var1: Formula, var2: Formula, var3: Formula, var4: Formula) = atLeast2Of4(var1, var2, var3, var4) && atLeast2Of4(!var1, !var2, !var3, !var4)

    val clauseSet = for {
      i <- 0 until 50
      j <- 0 until 80
    } yield {
      val c = input(i)(j)
      println("### " + c)
      c match {
        case "-1" => Set()
        case _ =>
            (i, j) match {
              case (0, 0) =>
                cnfToClauseSet(
                  c.toInt match {
                    case 0 => !vars(i)(j) && !vars(i + 1)(j) && !vars(i)(j + 1) && !vars(i + 1)(j + 1)
                    case 1 => choice4C1(vars(i)(j), vars(i + 1)(j), vars(i)(j + 1), vars(i + 1)(j + 1), false)
                    case 2 => choice4C2(vars(i)(j), vars(i + 1)(j), vars(i)(j + 1), vars(i + 1)(j + 1))
                    case 3 => choice4C1(vars(i)(j), vars(i + 1)(j), vars(i)(j + 1), vars(i + 1)(j + 1), true)
                    case 4 => vars(i)(j) && vars(i + 1)(j) && vars(i)(j + 1) && vars(i + 1)(j + 1)
                  }
                )
              case (0, 79) =>
                cnfToClauseSet(
                  c.toInt match {
                    case 0 => !vars(i)(j) && !vars(i + 1)(j) && !vars(i)(j - 1) && !vars(i + 1)(j - 1)
                    case 1 => choice4C1(vars(i)(j), vars(i + 1)(j), vars(i)(j - 1), vars(i + 1)(j - 1), false)
                    case 2 => choice4C2(vars(i)(j), vars(i + 1)(j), vars(i)(j - 1), vars(i + 1)(j - 1))
                    case 3 => choice4C1(vars(i)(j), vars(i + 1)(j), vars(i)(j - 1), vars(i + 1)(j - 1), true)
                    case 4 => vars(i)(j) && vars(i + 1)(j) && vars(i)(j - 1) && vars(i + 1)(j - 1)
                  }
                )
              case (49, 0) =>
                cnfToClauseSet(
                  c.toInt match {
                    case 0 => !vars(i)(j) && !vars(i - 1)(j) && !vars(i)(j + 1) && !vars(i - 1)(j + 1)
                    case 1 => choice4C1(vars(i)(j), vars(i - 1)(j), vars(i)(j + 1), vars(i - 1)(j + 1), false)
                    case 2 => choice4C2(vars(i)(j), vars(i - 1)(j), vars(i)(j + 1), vars(i - 1)(j + 1))
                    case 3 => choice4C1(vars(i)(j), vars(i - 1)(j), vars(i)(j + 1), vars(i - 1)(j + 1), true)
                    case 4 => vars(i)(j) && vars(i - 1)(j) && vars(i)(j + 1) && vars(i - 1)(j + 1)
                  }
                )
              case (49, 79) =>
                cnfToClauseSet(
                  c.toInt match {
                    case 0 => !vars(i)(j) && !vars(i - 1)(j) && !vars(i)(j - 1) && !vars(i - 1)(j - 1)
                    case 1 => choice4C1(vars(i)(j), vars(i - 1)(j), vars(i)(j - 1), vars(i - 1)(j - 1), false)
                    case 2 => choice4C2(vars(i)(j), vars(i - 1)(j), vars(i)(j - 1), vars(i - 1)(j - 1))
                    case 3 => choice4C1(vars(i)(j), vars(i - 1)(j), vars(i)(j - 1), vars(i - 1)(j - 1), true)
                    case 4 => vars(i)(j) && vars(i - 1)(j) && vars(i)(j - 1) && vars(i - 1)(j - 1)
                  }
                )
              case (0, _) =>
                cnfToClauseSet(
                  c.toInt match {
                    case 0 => row(i, j, true) && postRow(i, j, true)
                    case 1 => choice6C1(vars(i)(j - 1), vars(i)(j), vars(i)(j + 1), vars(i + 1)(j - 1), vars(i + 1)(j), vars(i + 1)(j + 1), false)
                    case 2 => choice6C2(vars(i)(j - 1), vars(i)(j), vars(i)(j + 1), vars(i + 1)(j - 1), vars(i + 1)(j), vars(i + 1)(j + 1), false)
                    case 3 => choice6C3(Seq(vars(i)(j - 1), vars(i)(j), vars(i)(j + 1), vars(i + 1)(j - 1), vars(i + 1)(j), vars(i + 1)(j + 1)))
                    case 4 => choice6C2(vars(i)(j - 1), vars(i)(j), vars(i)(j + 1), vars(i + 1)(j - 1), vars(i + 1)(j), vars(i + 1)(j + 1), true)
                    case 5 => choice6C1(vars(i)(j - 1), vars(i)(j), vars(i)(j + 1), vars(i + 1)(j - 1), vars(i + 1)(j), vars(i + 1)(j + 1), true)
                    case 6 => row(i, j, false) && postRow(i, j, false)
                  }
                )
              case (49, _) =>
                cnfToClauseSet(
                  c.toInt match {
                    case 0 => row(i, j, true) && postRow(i, j, true)
                    case 1 => choice6C1(vars(i - 1)(j - 1), vars(i - 1)(j), vars(i - 1)(j + 1), vars(i)(j - 1), vars(i)(j), vars(i)(j + 1), false)
                    case 2 => choice6C2(vars(i - 1)(j - 1), vars(i - 1)(j), vars(i - 1)(j + 1), vars(i)(j - 1), vars(i)(j), vars(i)(j + 1), false)
                    case 3 => choice6C3(Seq(vars(i - 1)(j - 1), vars(i - 1)(j), vars(i - 1)(j + 1), vars(i)(j - 1), vars(i)(j), vars(i)(j + 1)))
                    case 4 => choice6C2(vars(i - 1)(j - 1), vars(i - 1)(j), vars(i - 1)(j + 1), vars(i)(j - 1), vars(i)(j), vars(i)(j + 1), true)
                    case 5 => choice6C1(vars(i - 1)(j - 1), vars(i - 1)(j), vars(i - 1)(j + 1), vars(i)(j - 1), vars(i)(j), vars(i)(j + 1), true)
                    case 6 => preRow(i, j, false) && row(i, j, false)
                  }
                )
              case (_, 0) =>
                cnfToClauseSet(
                  c.toInt match {
                    case 0 => !vars(i - 1)(j) && !vars(i - 1)(j + 1) && !vars(i)(j) && !vars(i)(j + 1) && !vars(i + 1)(j) && !vars(i + 1)(j + 1)
                    case 1 => choice6C1(vars(i - 1)(j), vars(i - 1)(j + 1), vars(i)(j), vars(i)(j + 1), vars(i + 1)(j), vars(i + 1)(j + 1), false)
                    case 2 => choice6C2(vars(i - 1)(j), vars(i - 1)(j + 1), vars(i)(j), vars(i)(j + 1), vars(i + 1)(j), vars(i + 1)(j + 1), false)
                    case 3 => choice6C3(Seq(vars(i - 1)(j), vars(i - 1)(j + 1), vars(i)(j), vars(i)(j + 1), vars(i + 1)(j), vars(i + 1)(j + 1)))
                    case 4 => choice6C2(vars(i - 1)(j), vars(i - 1)(j + 1), vars(i)(j), vars(i)(j + 1), vars(i + 1)(j), vars(i + 1)(j + 1), true)
                    case 5 => choice6C1(vars(i - 1)(j), vars(i - 1)(j + 1), vars(i)(j), vars(i)(j + 1), vars(i + 1)(j), vars(i + 1)(j + 1), true)
                    case 6 => vars(i - 1)(j) && vars(i - 1)(j + 1) && vars(i)(j) && vars(i)(j + 1) && vars(i + 1)(j) && vars(i + 1)(j + 1)
                  }
                )
              case (_, 79) =>
                cnfToClauseSet(
                  c.toInt match {
                    case 0 => !vars(i - 1)(j - 1) && !vars(i - 1)(j) && !vars(i)(j - 1) && !vars(i)(j) && !vars(i + 1)(j - 1) && !vars(i + 1)(j)
                    case 1 => choice6C1(vars(i - 1)(j - 1), vars(i - 1)(j), vars(i)(j - 1), vars(i)(j), vars(i + 1)(j - 1), vars(i + 1)(j), false)
                    case 2 => choice6C2(vars(i - 1)(j - 1), vars(i - 1)(j), vars(i)(j - 1), vars(i)(j), vars(i + 1)(j - 1), vars(i + 1)(j), false)
                    case 3 => choice6C3(Seq(vars(i - 1)(j - 1), vars(i - 1)(j), vars(i)(j - 1), vars(i)(j), vars(i + 1)(j - 1), vars(i + 1)(j)))
                    case 4 => choice6C2(vars(i - 1)(j - 1), vars(i - 1)(j), vars(i)(j - 1), vars(i)(j), vars(i + 1)(j - 1), vars(i + 1)(j), true)
                    case 5 => choice6C1(vars(i - 1)(j - 1), vars(i - 1)(j), vars(i)(j - 1), vars(i)(j), vars(i + 1)(j - 1), vars(i + 1)(j), true)
                    case 6 => vars(i - 1)(j - 1) && vars(i - 1)(j) && vars(i)(j - 1) && vars(i)(j) && vars(i + 1)(j - 1) && vars(i + 1)(j)
                  }
                )
              case (_, _) => {
                c.toInt match {
                  case 0 => cnfToClauseSet(preRow(i, j, true) && row(i, j, true) && postRow(i, j, true))
                  case 1 => cnfToClauseSet(choice9C1(vars(i - 1)(j - 1),vars(i - 1)(j),vars(i - 1)(j + 1),vars(i)(j - 1), vars(i)(j), vars(i)(j + 1), vars(i + 1)(j - 1), vars(i + 1)(j), vars(i + 1)(j + 1), false))
                  case 2 => cnfToClauseSet(choice9C2(vars(i - 1)(j - 1),vars(i - 1)(j),vars(i - 1)(j + 1),vars(i)(j - 1), vars(i)(j), vars(i)(j + 1), vars(i + 1)(j - 1), vars(i + 1)(j), vars(i + 1)(j + 1), false))
                  case 3 => cnfToClauseSet(choice9C3(vars(i - 1)(j - 1),vars(i - 1)(j),vars(i - 1)(j + 1),vars(i)(j - 1), vars(i)(j), vars(i)(j + 1), vars(i + 1)(j - 1), vars(i + 1)(j), vars(i + 1)(j + 1), false))
                  case 4 => cnfToClauseSet(choice9C4(vars(i - 1)(j - 1),vars(i - 1)(j),vars(i - 1)(j + 1),vars(i)(j - 1), vars(i)(j), vars(i)(j + 1), vars(i + 1)(j - 1), vars(i + 1)(j), vars(i + 1)(j + 1), false))
                  case 5 => cnfToClauseSet(choice9C4(vars(i - 1)(j - 1),vars(i - 1)(j),vars(i - 1)(j + 1),vars(i)(j - 1), vars(i)(j), vars(i)(j + 1), vars(i + 1)(j - 1), vars(i + 1)(j), vars(i + 1)(j + 1), true))
                  case 6 => cnfToClauseSet(choice9C3(vars(i - 1)(j - 1),vars(i - 1)(j),vars(i - 1)(j + 1),vars(i)(j - 1), vars(i)(j), vars(i)(j + 1), vars(i + 1)(j - 1), vars(i + 1)(j), vars(i + 1)(j + 1), true))
                  case 7 => cnfToClauseSet(choice9C2(vars(i - 1)(j - 1),vars(i - 1)(j),vars(i - 1)(j + 1),vars(i)(j - 1), vars(i)(j), vars(i)(j + 1), vars(i + 1)(j - 1), vars(i + 1)(j), vars(i + 1)(j + 1), true))
                  case 8 => cnfToClauseSet(choice9C1(vars(i - 1)(j - 1),vars(i - 1)(j),vars(i - 1)(j + 1),vars(i)(j - 1), vars(i)(j), vars(i)(j + 1), vars(i + 1)(j - 1), vars(i + 1)(j), vars(i + 1)(j + 1), true))
                  case 9 => cnfToClauseSet(preRow(i, j, false) && row(i, j, false) && postRow(i, j, false))
                }
              }
            }
      }
    }
    // format: on

    println("##################")

    toCnfOutput(clauseSet.foldLeft(Set[Clause]()) {
      case (acc, s) => if (s.isEmpty) acc else acc ++ s
    })
  }

  def display_problem1_8050(): Unit = {
    val reader = getFileReader("miniSAT.txt")
    val header = reader.readLine()
    header match {
      case "UNSAT" => println("fail")
      case "SAT" => {
        val answer = reader.readLine().split(" ").filterNot(_ == "0")
        val output = answer.zipWithIndex.map {
          case (v, i) =>
            val n = i + 1
            val d = v.startsWith("-") match {
              case true  => "  "
              case false => "■■"
            }
            if (n % 80 == 0) d + "\n" else d
        }
        println(output.mkString(""))
      }
    }
  }
}
