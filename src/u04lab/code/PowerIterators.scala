package u04lab.code

import Optionals._
import Lists._
import Lists.List.{Cons, _}
import Streams.Stream._
import Streams._

import scala.util.Random

trait PowerIterator[A] {
  def next(): Option[A]
  def allSoFar(): List[A]
  def reversed(): PowerIterator[A]

}

trait PowerIteratorsFactory {

  def incremental(start: Int, successive: Int => Int): PowerIterator[Int]
  def fromList[A](list: List[A]): PowerIterator[A]
  def randomBooleans(size: Int): PowerIterator[Boolean]
}



class PowerIteratorsFactoryImpl extends PowerIteratorsFactory {

  override def incremental(start: Int, successive: Int => Int): PowerIterator[Int] = fromStream(iterate(start)(successive))

  override def fromList[A](list: List[A]): PowerIterator[A] = fromStream(toStream(list))

  override def randomBooleans(size: Int): PowerIterator[Boolean] = fromStream(take(generate(Random.nextBoolean()))(size))

  def toStream[A](l: List[A]): Stream[A] = l match {
    case Cons(h, t) => Stream.cons(h, toStream(t))
    case Nil() =>Empty()
  }

  private def fromStream[A](stream: Stream[A]): PowerIterator[A] = new PowerIterator[A] {
    var futureStream: Stream[A] = stream
    var pastList: List[A] = Nil()

    override def next(): Option[A] = futureStream match {
      case Stream.Cons(h, t) => futureStream = t(); pastList = append(pastList, Cons(h(), Nil())); Option.of(h())
      case Stream.Empty() => Option.empty
    }

    override def allSoFar(): List[A] = pastList

    override def reversed(): PowerIterator[A] = fromStream(toStream(reverse(pastList)))
  }
}
