import scala.annotation.tailrec

def oczysc[A](l: List[A]): List[A] = {
  @tailrec
  def helper[A](lista: List[A], poprzedni: Option[A] = None, wynik: List[A] = List()): List[A] = {
    lista match {
      case element :: reszta => {
        if (poprzedni.nonEmpty) {
          if (element != poprzedni.get) helper(reszta, Some(element), wynik :+ element)
          else helper(reszta, poprzedni, wynik)
        } else helper(reszta, Some(element), wynik :+ element)
      }
      case _ => wynik
    }
  }
  helper(l)
}

@main def lab5zad1(): Unit = {
  println(oczysc(List(1, 1, 2, 4, 4, 4, 1, 3)))
}
