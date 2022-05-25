def remElems[A](seq: Seq[A], k: Int): Seq[A] = {
  val zipped = seq.zipWithIndex
  val filtered = zipped.filter((a, b) => b != k - 1)
  val result = filtered.map((a, b) => a)
  result
}

@main def lab6zad4(): Unit = {
  println(remElems(Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 5))
}
