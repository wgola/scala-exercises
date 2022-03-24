import scala.annotation.tailrec

def tasuj(l1: List[Int], l2: List[Int]): List[Int] = {
    @tailrec    
    def pomocnicza(lista1: List[Int], lista2: List[Int], wynik: List[Int] = List(), poprzedni: Int = 0, licznik: Int = 0): List[Int] = {
        if (licznik == 0) {
            if (lista1.isEmpty && lista2.isEmpty) wynik
            else if (lista1.nonEmpty && lista2.isEmpty) pomocnicza(lista1.tail, lista2, wynik :+ lista1.head, lista1.head, licznik + 1)
            else if (lista1.isEmpty && lista2.nonEmpty) pomocnicza(lista1, lista2.tail, wynik :+ lista2.head, lista2.head, licznik + 1)
            else if (lista1.head < lista2.head) pomocnicza(lista1.tail, lista2, wynik :+ lista1.head, lista1.head, licznik + 1)
            else pomocnicza(lista1, lista2.tail, wynik :+ lista2.head, lista2.head, licznik + 1)
        }
        else {
            if (lista1.isEmpty && lista2.isEmpty) wynik
            else if (lista1.nonEmpty && lista2.isEmpty) {
                if (lista1.head != poprzedni) pomocnicza(lista1.tail, lista2, wynik :+ lista1.head, lista1.head, licznik + 1)
                else pomocnicza(lista1.tail, lista2, wynik, poprzedni, licznik + 1)
            }
            else if (lista1.isEmpty && lista2.nonEmpty) {
                if (lista2.head != poprzedni) pomocnicza(lista1, lista2.tail, wynik :+ lista2.head, lista2.head, licznik + 1)
                else pomocnicza(lista1, lista2.tail, wynik, poprzedni, licznik + 1)
            }
            else if (lista1.head < lista2.head) {
                if (lista1.head != poprzedni) pomocnicza(lista1.tail, lista2, wynik :+ lista1.head, lista1.head, licznik + 1)
                else pomocnicza(lista1.tail, lista2, wynik, poprzedni, licznik + 1)
            }
            else {
                if (lista2.head != poprzedni) pomocnicza(lista1, lista2.tail, wynik :+ lista2.head, lista2.head, licznik + 1)
                else pomocnicza(lista1, lista2.tail, wynik, poprzedni, licznik + 1)
            }
        }
    }
    pomocnicza(l1, l2)
}

@main def lab3zad4(): Unit = {
   println(tasuj(List(2, 4, 3, 5), List(1, 2, 2, 3, 1, 5)))
}
