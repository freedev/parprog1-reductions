package reductions

import org.scalameter._
import common._

object ParallelCountChangeRunner {

  @volatile var seqResult = 0

  @volatile var parResult = 0

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 80,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val amount = 250
    val coins = List(1, 2, 5, 10, 20, 50)
    val seqtime = standardConfig measure {
      seqResult = ParallelCountChange.countChange(amount, coins)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential count time: $seqtime ms")

    def measureParallelCountChange(threshold: ParallelCountChange.Threshold): Unit = {
      val fjtime = standardConfig measure {
        parResult = ParallelCountChange.parCountChange(amount, coins, threshold)
      }
      println(s"parallel result = $parResult")
      println(s"parallel count time: $fjtime ms")
      println(s"speedup: ${seqtime / fjtime}")
    }

    measureParallelCountChange(ParallelCountChange.moneyThreshold(amount))
    measureParallelCountChange(ParallelCountChange.totalCoinsThreshold(coins.length))
    measureParallelCountChange(ParallelCountChange.combinedThreshold(amount, coins))
  }
}

object ParallelCountChange {

  /** Returns the number of ways change can be made from the specified list of
   *  coins for the specified amount of money.
   */
  def countCoins(s: List[Int], rest: Int, counter: Int): Int = {
    if (!s.isEmpty && rest >= 0) {
      val curRest = rest - s.head
      if (!(curRest < 0)) {
        if (curRest >= s.head) {
          if (s.size > 1) {
            countCoins(s, curRest, countCoins(s.tail, curRest, counter) )
          } else
            countCoins(s, curRest, counter)
        } else if (curRest == 0 && s.size == 1) {
           counter + 1
        } else
           countCoins(s.tail, curRest, counter)
      } else
        counter
    } else
      counter
  }

  def innerGetCombinations(prefix: List[Int], s: List[Int], depth: Int, threshold: Threshold): List[List[Int]] = {
    if (s.size > 0) {
      val curComb = s.head :: prefix
      val (l1, l2) = (innerGetCombinations(curComb, s.tail, depth + 1, threshold), innerGetCombinations(prefix, s.tail, depth + 1, threshold))
      (l1, l2) match {
        case (Nil, Nil) => List(curComb) 
        case (x1, Nil) => List(curComb) ::: x1
        case (Nil, x2) => List(curComb) ::: x2
        case (x1, x2) => List(curComb) ::: x1 ::: x2
      }
    } else {
      Nil
    }
  }

  def countChange(money: Int, coins: List[Int]): Int = {
      val threshold: Threshold = (a, b) => { false }
      parCountChange(money, coins, threshold)
  }

  type Threshold = (Int, List[Int]) => Boolean

  /** In parallel, counts the number of ways change can be made from the
   *  specified list of coins for the specified amount of money.
   */
  def parCountChange(money: Int, coins: List[Int], threshold: Threshold): Int = {
    if (money == 0) {
      1
    } else {
      def iter(coins: List[List[Int]], counter: Int): Int = {
        coins match {
          case Nil => counter
          case x :: xs => iter(xs, countCoins(x, money, counter))
        }
      }
      iter(innerGetCombinations(List(), coins.sorted, 0, threshold), 0)
    }
  }

  /** Threshold heuristic based on the starting money. */
  def moneyThreshold(startingMoney: Int): Threshold =
    ???

  /** Threshold heuristic based on the total number of initial coins. */
  def totalCoinsThreshold(totalCoins: Int): Threshold =
    ???


  /** Threshold heuristic based on the starting money and the initial list of coins. */
  def combinedThreshold(startingMoney: Int, allCoins: List[Int]): Threshold = {
    ???
  }
}
