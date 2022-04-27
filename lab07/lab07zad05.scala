def swap[A](l: List[A]): List[A] = {
    val x = l.sliding(2, 2)
    def helper(pary: Iterator[Seq[A]], wynik: List[A] = List()): List[A] = {
        if (pary.nonEmpty) { pary.next match {
            case Seq(a, b) => helper(pary, wynik :+ b :+ a)
            case Seq(a) => helper(pary, wynik :+ a)
            }
        }
        else wynik
    }
    helper(x)
}

@main def lab7zad5(): Unit = {
    val lista = List(1, 2, 3, 4, 5)
    println(swap(lista)) // ==> List(2, 1, 4, 3, 5)
}