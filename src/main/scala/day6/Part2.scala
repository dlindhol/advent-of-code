package day6

import scala.collection.immutable.Queue

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import fs2._

object Part2 extends IOApp {

  /**
   * Converts a list of fish timer settings into a Queue
   * that manages the count of fish in each stage of the cycle.
   * as indicated by the position in the queue. For example,
   * the head of the queue is the number of fish that will
   * spawn in the next step. The length of the queue is such
   * that new fish starting at the end will spawn when they
   * come of age. Using a Queue makes it easier to effectively
   * decrement every fish's timer.
   */
  def count(s: School): Queue[Long] = {
    val counts = Array.fill(9)(0L)
    s.foreach { n => counts.update(n, counts(n) + 1) }
    Queue.from(counts)
  }

  /**
   * Dequeues the spawning fish and returns them to position 6
   * in the queue to reset their cycle. The newly spawned fish
   * will be added to the queue in position 8.
   */
  def step(counts: Queue[Long]): Queue[Long] =
    counts.dequeue match { case (n, q) =>
      val a = q.toArray
      a.update(6, a(6) + n) //hack spawning fish into position 6
      Queue.from(a).enqueue(n) //new fish with timer set to 8
    }

  /** Evolve n steps. */
  def evolve(counts: Queue[Long], n: Int): Queue[Long] =
    Stream.iterate(counts)(step)
      .take(n + 1) //n steps beyond the initial state
      .lastOr(???)
      .compile.toList.head

  /**
   * Converts the data to the counting queue, evolves it through
   * the given number of steps, then adds up the number of fish.
   */
  def process(steps: Int): Pipe[IO, Data, Result] = {
    _.map(count)
      .map { counts =>
        evolve(counts, steps)
      }
      .map(_.sum)
  }

  /** Stitch together the processing steps. */
  def solve(data: Stream[IO, String], steps: Int): IO[Result] =
    data
      .through(clean)
      .through(parse)
      .through(process(steps))
      .compile.toList.map(_.head)


  def run(args: List[String]): IO[ExitCode] = {
    solve(data, 256)
      .map(println)
      .as(ExitCode.Success)
  }
}
