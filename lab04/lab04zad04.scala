import scala.annotation.tailrec

def divide[A](l: List[A]): (List[A], List[A]) = {
  @tailrec
  def helper(lista: List[A], indeks: Int, wynik1: List[A], wynik2: List[A]): (List[A], List[A]) =
    lista match {
      case glowa :: ogon => {
        if (indeks % 2 == 0) helper(ogon, indeks + 1, wynik1 :+ glowa, wynik2)
        else helper(ogon, indeks + 1, wynik1, wynik2 :+ glowa)
      }
      case List() => (wynik1, wynik2)
    }
  helper(l, 0, List(), List())
}

@main def lab4zad4(): Unit = {
  println(divide(List(1, 3, 5, 6, 7)))
}
