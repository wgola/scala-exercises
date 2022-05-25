import scala.annotation.tailrec

def jestPierwsza(n: Int): Boolean = {
  @tailrec
  def ileDzielnikow(liczba: Int, dzielnik: Int = 1, ilosc: Int = 0): Int = {
    if (dzielnik > liczba) ilosc
    else if (liczba % dzielnik == 0) ileDzielnikow(liczba, dzielnik + 1, ilosc + 1)
    else ileDzielnikow(liczba, dzielnik + 1, ilosc)
  }
  if (ileDzielnikow(n) == 2) true
  else false
}

@main def lab3zad2(): Unit = {
  println(jestPierwsza(4))
}
