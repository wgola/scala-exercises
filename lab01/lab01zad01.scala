def obramuj(napis: String): String = {
   val linijki: Array[String] = napis.split('\n')
   val najdluzsza: Int = linijki.maxBy(s => s.length).length
   var wynik: String = "*" * (najdluzsza + 4) + '\n'
   linijki.map(s => "* " + s + " " * (najdluzsza - s.length) + " *").foreach(s => wynik += s + '\n')
   wynik += "*" * (najdluzsza + 4)
   wynik
}

@main def lab1zad1(): Unit = {
   val dane: String = "Przykladowy tekst\nzawierajacy\nwiecej niz\n3 linijki."
   println(obramuj(dane))
}
