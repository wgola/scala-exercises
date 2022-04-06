import scala.annotation.tailrec

def applyForAll[A, B](l: List[A])(f: A => B): List[B] = {
    @tailrec
    def helper(lista: List[A], funkcja: A => B, wynik: List[B] = List()): List[B] = {
        lista match {
            case element :: reszta => helper(reszta, funkcja, wynik :+ funkcja(element))
            case _ => wynik
        }
    }
    helper(l, f)
}

@main def lab5zad4(): Unit = {

    val l = List(1, 3, 5)
    val f = (n: Int) => n + 3

    println(applyForAll(l)(f))

}