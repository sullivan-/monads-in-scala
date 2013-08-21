package monads.lisst

import scala.annotation.tailrec

object Lisst {
 
  def apply[A](elems: A*): Lisst[A] = reverseSeq(elems.reverse)

  private def reverseSeq[A](elems: Seq[A]): Lisst[A] = {
    var result: Lisst[A] = EmptyLisst
    elems.foreach { elem => result = NonEmptyLisst(elem, result) }
    result    
  }

}
 
sealed trait Lisst[+A] {

  def flatMap[B](f: A => Lisst[B]): Lisst[B] = {
    var reverseBs = List[B]()
    this.foreach { a =>
      f(a).foreach { b =>
        reverseBs = b +: reverseBs
      }
    }
    Lisst.reverseSeq(reverseBs)
  }

  /** Walks through the lisst, applying function f to every element in turn. */
  @tailrec
  final def foreach(f: A => Unit): Unit = this match {
    case NonEmptyLisst(head, tail) => {
      f(head)
      tail.foreach(f)
    }
    case EmptyLisst =>
  }

  def map[B](f: A => B): Lisst[B] = flatMap { a => Lisst(f(a)) }

  def flatten[B](implicit asLisstLisst: Lisst[A] <:< Lisst[Lisst[B]]): Lisst[B] = 
    asLisstLisst(this) flatMap identity

  override def toString: String = {
    val sb = new StringBuilder
    sb.append("Lisst(")
    foreach { a => sb.append(a).append(", ") }
    this match {
      case NonEmptyLisst(_, _) => sb.delete(sb.length - 2, sb.length)
      case _ =>
    }
    sb.append(")")
    sb.toString
  }

}

private case class NonEmptyLisst[+A](
  head: A,
  tail: Lisst[A])
extends Lisst[A]

private case object EmptyLisst
extends Lisst[Nothing]
