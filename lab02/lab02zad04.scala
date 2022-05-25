def hipoteza(n: Int): Unit = {
  val liczba: Int = n

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

  @annotation.tailrec
  def sprawdzenie(a: Int = 1, b: Int = liczba - 1): Int = {
    if (pierwsza(a) && pierwsza(b)) a
    else if (a >= liczba || b <= 0) 0
    else sprawdzenie(a + 1, b - 1)
  }

  if (sprawdzenie() == 0) println("Nie znaleziono takich liczb")
  else println(liczba + " == " + sprawdzenie() + " + " + (liczba - sprawdzenie()))
}

@main def lab2zad4(n: Int): Unit = {
  if (n % 2 == 0 && n > 2) hipoteza(n)
  else println("Liczba " + n + " nie jest parzysta lub jest nie większa niż 2")
}
