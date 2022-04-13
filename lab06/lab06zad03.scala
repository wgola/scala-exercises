def deStutter[A](seq: Seq[A]): Seq[A] = {
    seq.foldLeft(List[A]()) ( (acc, elem) => {
        if (acc.nonEmpty && acc.last == elem) acc
        else acc :+ elem
    } )
}

@main def lab6zad3(): Unit = {
    println(deStutter(Seq(1, 1, 2, 4, 4, 4, 1, 3)))
}