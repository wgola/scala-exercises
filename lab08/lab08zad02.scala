@main def lab8zad2(): Unit = {
    val dane = List(
        ("Jan", "Kowalski", 5.5, 15.0),
        ("Adam", "Czarnecki", 10.5, 11.0),
        ("Jakub", "Klimczewski", 8.0, 17.5),
        ("Jan", "Kowalski", 15.0, 15.5),
        ("Piotr", "Nowak", 9.5, 18.5),
        ("Marek", "Nazwisko", 10.0, 5.5),
        ("Piotr", "Nowak", 1.5, 2.5),
        ("Ewa", "Kowalska", 5.5, 7.5),
        ("Adam", "Czarnecki", 8.5, 11.5),
        ("Piotr", "Nowak", 4.5, 14.5),
        ("Ewa", "Kowalska", 4.5, 2.5)
    )

    val test = dane.groupMap( (imie, nazwisko, wdziek, spryt) => imie + " " + nazwisko)( (imie, nazwisko, wdziek, spryt) => (wdziek, spryt))
    
    val srednie = for {
        (imie, lista) <- test
    } yield {
        val ile = lista.size
        val tmp = lista.foldLeft((0.0, 0.0))( (acc, elem) => (acc(0) + elem(0), acc(1) + elem(1)))
        val wyniki = List(tmp(0) / ile, tmp(1) / ile)
        (imie, wyniki(0), wyniki(1), wyniki(0) + wyniki(1))
    }

    println(srednie)

    val posortowane = srednie.toList.sortWith( (elem1, elem2) => {
        if (elem1(3) > elem2(3)) true
        else if (elem1(3) == elem2(3)) {
            if (elem1(1) > elem2(1)) true
            else if (elem1(1) == elem2(1)) {
                elem1(0) > elem2(0)
            }
            else false
        }
        else false
    })

    val wynik = posortowane.zipWithIndex.map( n => n match {
        case ((imie, wdziek, spryt, suma), indeks) => (indeks+1, imie, suma)
    })

    println(wynik)
}