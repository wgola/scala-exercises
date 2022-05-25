import scala.annotation.tailrec

def isOrdered2[A](seq: Seq[A])(leq: (A, A) => Boolean): Boolean = {
  val pary = seq.sliding(2, 1)
  @tailrec
  def helper(
      elementy: Iterator[Seq[A]],
      funkcja: (A, A) => Boolean,
      wynik: Boolean = true
  ): Boolean = {
    if (elementy.nonEmpty) {
      val para = elementy.next
      if (funkcja(para(0), para(1))) helper(elementy, funkcja, true)
      else false
    } else wynik
  }
  helper(pary, leq)
}

@main def lab6zad5(): Unit = {
  println(isOrdered2(Seq(1, 2, 2, 4))(_ <= _))
}
