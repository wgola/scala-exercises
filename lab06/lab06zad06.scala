def freqMax[A](list: List[A]): (Set[A], Int) = {
  val elementIlosc = list.map(n => (n, list.count(b => b == n)))
  val maks = list.foldLeft(0)((acc, elem) => {
    val tmp = list.count(n => n == elem)
    if (tmp > acc) tmp
    else acc
  })
  val filtered = elementIlosc.filter((a, b) => b == maks)
  val lista = filtered.map((a, b) => a)
  val zbior = lista.foldLeft(Set[A]())((acc, elem) => acc + elem)
  (zbior, maks)
}

@main def lab6zad6(): Unit = {
  val l = List(1, 1, 2, 4, 4, 3, 4, 1, 3)
  println(freqMax(l) == (Set(1, 4), 3))
}
