import scala.annotation.tailrec

def skompresuj[A](l: List[A]): List[(A, Int)] = {
  @tailrec
  def helper[A](
      lista: List[A],
      wynik: List[(A, Int)] = List(),
      tymczasowa: Int = 0,
      poprzedni: Option[A] = None
  ): List[(A, Int)] = {
    lista match {
      case element :: reszta => {
        if (poprzedni.nonEmpty) {
          if (element == poprzedni.get) helper(reszta, wynik, tymczasowa + 1, poprzedni)
          else helper(lista, wynik :+ (poprzedni.get, tymczasowa), 0, None)
        } else helper(reszta, wynik, tymczasowa + 1, Some(element))
      }
      case _ => wynik :+ (poprzedni.get, tymczasowa)
    }
  }
  helper(l)
}

@main def lab5zad2(): Unit = {
  println(skompresuj(List('a', 'a', 'b', 'c', 'c', 'c', 'a', 'a', 'b', 'd')))
}
