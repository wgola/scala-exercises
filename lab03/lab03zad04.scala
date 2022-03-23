def tasuj(l1: List[Int], l2: List[Int]): List[Int] = {
    def pomocnicza(lista1: List[Int], lista2: List[Int], akum: List[Int] = List(), poprzedni: Int = 0): List[Int] = {
        if (lista1.length == 0 && lista2.length == 0) akum
        else if (lista1.head < lista2.head && lista1.head != poprzedni) pomocnicza(lista1.tail, lista2, akum :+ lista1.head, lista1.head)
        else if (lista2.head < lista1.head && lista2.head != poprzedni) pomocnicza(lista1, lista2.tail, akum :+ lista2.head, lista2.head)
        else pomocnicza(lista1.tail, lista2.tail, akum :+ lista1.head :+ lista2.head)
    }
    pomocnicza(l1, l2)
}



@main def lab3zad4(): Unit = {
   println(tasuj(List(1, 2, 3), List(4, 5, 6)))
}