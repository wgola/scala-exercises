def indices[A](l: List[A], el: A): Set[Int] = {
    val x = l.zipWithIndex
    val y = x.groupMap(elem => elem(0))(elem => elem(1)).filter(elem => elem(0) == el)
    if (y.nonEmpty) y(el).toSet
    else Set()
}

@main def lab7zad4(): Unit = {
    val lista = List(1, 2, 1, 1, 5)
    println(indices(lista, 1))
    println(indices(lista, 7))
}