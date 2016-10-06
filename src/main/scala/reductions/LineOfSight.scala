package reductions

import org.scalameter._
import common._

object LineOfSightRunner {
  
  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 100,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]) {
    val length = 10000000
    val input = (0 until length).map(_ % 100 * 1.0f).toArray
    val output = new Array[Float](length + 1)
    val seqtime = standardConfig measure {
      LineOfSight.lineOfSight(input, output)
    }
    println(s"sequential time: $seqtime ms")

    val partime = standardConfig measure {
      LineOfSight.parLineOfSight(input, output, 10000)
    }
    println(s"parallel time: $partime ms")
    println(s"speedup: ${seqtime / partime}")
  }
}

object LineOfSight {

  def max(a: Float, b: Float): Float = if (a > b) a else b

  def lineOfSight(input: Array[Float], output: Array[Float]): Unit = {
    downsweepSequential(input, output, 0, 0, input.size)
  }

  sealed abstract class Tree {
    def maxPrevious: Float
  }

  case class Node(left: Tree, right: Tree) extends Tree {
    val maxPrevious = max(left.maxPrevious, right.maxPrevious)
  }

  case class Leaf(from: Int, until: Int, maxPrevious: Float) extends Tree

  /** Traverses the specified part of the array and returns the maximum angle.
   */
  def upsweepSequential(input: Array[Float], from: Int, until: Int): Float = {
    (for {
      i <- from until until
    } yield {
      if (i == 0) 
        0
      else
        input(i) / i
    }).max
  }

  /** Traverses the part of the array starting at `from` and until `end`, and
   *  returns the reduction tree for that part of the array.
   *
   *  The reduction tree is a `Leaf` if the length of the specified part of the
   *  array is smaller or equal to `threshold`, and a `Node` otherwise.
   *  If the specified part of the array is longer than `threshold`, then the
   *  work is divided and done recursively in parallel.
   */
  def upsweep(input: Array[Float], from: Int, end: Int,
    threshold: Int): Tree = {
      val mid = (from + end) / 2
      if (threshold >= (end - from)) {
        Leaf(from, end, upsweepSequential(input, from, end))
      } else {
        parallel(upsweep(input, from, mid, threshold), upsweep(input, mid, end, threshold)) match {
          case (x, y) => Node(x, y)
        }
      }
  }

  /** Traverses the part of the `input` array starting at `from` and until
   *  `until`, and computes the maximum angle for each entry of the output array,
   *  given the `startingAngle`.
   */
  def downsweepSequential(input: Array[Float], output: Array[Float],
    startingAngle: Float, from: Int, until: Int): Unit = {
    (for {
      i <- from until until
    } yield {
      if (i == 0) {
        output(i) = startingAngle
      } else {
        val res = input(i) / i
        output(i) = output(i-1) max res
      }
    })
  }

  /*
  [Test Description] downsweep should correctly compute the output for a non-zero starting angle
  [Observed Error] List(0.0, 7.0, 7.0, 11.0, 12.0) did not equal List(0.0, 8.0, 8.0, 11.0, 12.0)
  [Lost Points] 2
   */
  /** Pushes the maximum angle in the prefix of the array to each leaf of the
   *  reduction `tree` in parallel, and then calls `downsweepTraverse` to write
   *  the `output` angles.
   */
  def downsweep(input: Array[Float], output: Array[Float], startingAngle: Float,
    tree: Tree): Unit = {
    
    tree match {
      case n: Node => {
        parallel(downsweep(input, output, startingAngle max n.left.maxPrevious, n.left), downsweep(input, output, startingAngle max n.right.maxPrevious, n.right))
      }
      case l: Leaf => downsweepSequential(input, output, startingAngle max l.maxPrevious, l.from, l.until)
    }
  }

  /*
[Test Description] parLineOfSight should invoke the parallel construct 30 times (15 times during upsweep and 15 times during downsweep) for an array of size 17, with threshold 1
[Observed Error] 32 did not equal 30 (The number of parallel calls should be 30)
[Lost Points] 3

[Test Description] parLineOfSight should call parallel constuct 6 times, where the last two parallel constructs should update the 4 sections of the array (1 until 5), (5 until 9), (9 until 13), (13 until 17), respectively
[Observed Error] success was false [During the execution of first part of 5th call to parallel construct, the indices 1 until 5 of the output array was not correctly updated]
[Lost Points] 6
   */
  
  /** Compute the line-of-sight in parallel. */
  def parLineOfSight(input: Array[Float], output: Array[Float],
    threshold: Int): Unit = {
    downsweep(input, output, 0, upsweep(input, 1, input.size, threshold))
  }
}
