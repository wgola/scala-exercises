def position[A](l: List[A], el: A): Option[Int] = {
    val indeks = l.takeWhile(n => n != el)
    val odp = indeks.size
    if (odp == l.size) None
    else Some(odp)
}

@main def lab7zad3(): Unit = {
    val lista = List(2, 1, 1, 5)
    println(position(lista, 3))
}