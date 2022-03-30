import scala.annotation.tailrec

def usun[A](l: List[A], el: A): List[A] = {
    @tailrec
    def helper[A](lista: List[A], element: A, wynik: List[A]): List[A] = lista match {
        case glowa :: ogon => {
            if (glowa == element) helper(ogon, element, wynik)
            else helper(ogon, element, wynik :+ glowa)
        }
        case List() => wynik
    }
    helper(l, el, List())
}


@main def lab4zad3(): Unit = {
    println(usun(List(2, 1, 4, 1, 3, 3, 1, 2), 1))
}