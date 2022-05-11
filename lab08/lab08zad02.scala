@main def lab8zad2(): Unit = {
    val dane = List(
        ("Jan", "Kowalski", 5, 15),
        ("Adam", "Czarnecki", 10.3, 11.2),
        ("Jakub", "Klimczewski", 8.2, 17.4),
        ("Jan", "Kowalski", 10.4, 15.4),
        ("Piotr", "Nowak", 9.7, 18.5),
        ("Marek", "nazwisko", 10.1, 5.4),
        ("Piotr", "Nowak", 1.4, 2.4),
        ("Ewa", "Kowalska", 5.4, 7.4),
        ("Adam", "Czarnecki", 8.4, 11.4),
        ("Piotr", "Nowak", 4.4, 14.4)
    )

    val test = dane.groupMap( (imie, nazwisko, wdziek, spryt) => imie + " " + nazwisko) ( (imie, nazwisko, wdziek, spryt) => List(wdziek, spryt))

    val test2 = for {
        (imie, lista) <- test
    } yield {
        lista.foldLeft(List())((acc, elem) => List(
            acc.head + elem.head, acc.tail + elem.tail
        ))
    }

}