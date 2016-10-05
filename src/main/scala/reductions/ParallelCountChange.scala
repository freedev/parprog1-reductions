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
      coins match {
        case Nil => 0
        case x :: xs => {
          if (money < 0) {
            0
          } else {
            val rest = money - x
            if (rest < 0) {
              0
            } else if (rest > 0) {
              if (threshold(money, coins)) {
                val (p1, p2) = parallel(parCountChange(rest, coins, threshold), parCountChange(money, xs, threshold))
                p1 + p2
              } else {
                parCountChange(rest, coins, threshold) + parCountChange(money, xs, threshold)
              }
            } else {
              1
            }
          }
        }
      }
    }
  }

  /*
[Test Description] moneyThreshold should return true when the money is equal to two-thirds of the starting money
[Observed Error] false did not equal true moneyThreshold should return true, hint: starting money: 3
[Lost Points] 1
   */
  /** Threshold heuristic based on the starting money. */
  def moneyThreshold(startingMoney: Int): Threshold = {
    // moneyThreshold should return false when the money is greater than two-thirds of the starting money
    val threshold: Threshold = (a, b) => {
      val twoThirdsStartingMoney = ((startingMoney*2)/3)
//      println(((a*2)/3) + " " + startingMoney)
      a > twoThirdsStartingMoney
    }
    threshold
  }

  /*
[Test Description] totalCoinsThreshold should return false when the number of coins is greater than two-thirds of the initial number of coins
[Observed Error] true did not equal false totalCoinsThreshold should return false, hint: initial number of coins: 3
[Test Description] totalCoinsThreshold should return true when the number of coins is < two-thirds of the initial number of coins
[Observed Error] false did not equal true totalCoinsThreshold should return true, hint: initial number of coins: 3
[Lost Points] 1

[Test Description] totalCoinsThreshold should return true when the number of coins is equal to two-thirds of the initial number of coins
[Observed Error] false did not equal true totalCoinsThreshold should return true, hint: initial number of coins: 3
[Lost Points] 1

   */
  
  /** Threshold heuristic based on the total number of initial coins. */
  def totalCoinsThreshold(totalCoins: Int): Threshold = {
    val threshold: Threshold = (a, b) => {
      val twoThirdsTotalCoins = ((totalCoins*2)/3)
//      println(((a*2)/3) + " " + startingMoney)
      b.size > twoThirdsTotalCoins
    }
    threshold
  }

// combinedThreshold should return false when the number of coins times money greater than half of the initial number of coins times starting money
// combinedThreshold should return true when the number of coins times money is less than or equal to half of the initial number of coins times starting money
  /** Threshold heuristic based on the starting money and the initial list of coins. */
  def combinedThreshold(startingMoney: Int, allCoins: List[Int]): Threshold = {
    val threshold: Threshold = (a, b) => {
      // (b.size * a) >= ((allCoins.size * startingMoney)/2)
      moneyThreshold(startingMoney).apply(a, b) && totalCoinsThreshold(allCoins.size).apply(a, b)
    }
    threshold
  }
}
