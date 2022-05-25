def pierwsza(n: Int): Boolean = {
  @annotation.tailrec
  def ileDzielnikow(liczba: Int, dzielnik: Int = 1, ilosc: Int = 0): Int = {
    if (dzielnik > liczba) ilosc
    else if (liczba % dzielnik == 0) ileDzielnikow(liczba, dzielnik + 1, ilosc + 1)
    else ileDzielnikow(liczba, dzielnik + 1, ilosc)
  }
  if (ileDzielnikow(n) == 2) true
  else false
}

@main def lab2zad3(n: Int): Unit = {
  println("Podany argument: " + n)
  val wynik: Boolean = pierwsza(n)
  if (wynik) println("Twoja liczba jest pierwsza")
  else println("Twoja liczba nie jest pierwsza")
}
