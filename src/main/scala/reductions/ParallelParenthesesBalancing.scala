package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
      
      def loop(idx: Int, until: Int, acc: Int): Int = {
        if (idx < until) {
          val head = chars(idx)
          if (head == '(') {
            loop(idx + 1, until, acc + 1)
          } else if (head == ')') {
            if (acc > 0)
              loop(idx + 1, until, acc - 1)
            else 
              -1
          } else
            loop(idx + 1, until, acc)
        } else
          acc
      }
      loop(0, chars.size, 0) == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, acc: Int, arg2: Boolean): Int = {
         if (idx < until) {
          val head = chars(idx)
          if (head == '(') {
            traverse(idx + 1, until, acc + 1, arg2)
          } else if (head == ')') {
            if (acc > 0 || arg2)
              traverse(idx + 1, until, acc - 1, arg2)
            else 
              -1
          } else
            traverse(idx + 1, until, acc, arg2)
        } else
          acc
    }

    def reduce(from: Int, until: Int) : Int = {
      val mid = (from + until) / 2
      if (threshold >= (until - from)) {
        traverse(from, until, 0, (from > 0))
      } else {
        val (p1, p2) = parallel(reduce(from, mid), reduce(mid, until))
        p1 + p2
      }
    }

    reduce(0, chars.length) == 0
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
