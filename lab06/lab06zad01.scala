import scala.annotation.tailrec

def subSeq[A](seq: Seq[A], begIdx: Int, endIdx: Int): Seq[A] = {
  @tailrec
  def helper(sekwencja: Seq[A], poczatek: Int, koniec: Int, indeks: Int = 0): Seq[A] = {
    if (indeks < poczatek) helper(sekwencja.drop(1), poczatek, koniec, indeks + 1)
    else sekwencja.take(koniec - poczatek)
  }
  helper(seq, begIdx, endIdx)
}

@main def lab6zad1(): Unit = {
  println(subSeq(Seq(1, 2, 3, 4, 5), 1, 3))
}
