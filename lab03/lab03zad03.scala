import scala.annotation.tailrec

def ciag(n: Int): Int = {
  @tailrec
  def dodawanie(
      granica: Int,
      licznik: Int = 2,
      zero: Int = 2,
      jeden: Int = 1,
      wynik: Int = 3
  ): Int = {
    if (licznik == granica) wynik
    else dodawanie(granica, licznik + 1, jeden, wynik, jeden + wynik)
  }

  if (n == 0) 2
  else if (n == 1) 1
  else dodawanie(n)
}

@main def lab3zad3(n: Int): Unit = {
  println(ciag(n))
}
