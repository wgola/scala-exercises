object zad01 {
   def obramuj(napis: String): String = {
      val linijki: Array[String] = napis.split('\n')
      val najdluzsza: Int = linijki.maxBy(s => s.length).length
      var wynik: String = ""
      for (i <- 1 to najdluzsza + 4) {
         wynik += "*"
      }
      wynik += "\n"
      linijki.foreach(element =>
         wynik += "* " + element
         for (j <- 1 to (najdluzsza - element.length)) {
            wynik += " "
         }
         wynik += " *\n"
      )
      for (i <- 1 to najdluzsza + 4) {
         wynik += "*"
      }
      return wynik
   }

   def main(args: Array[String]): Unit = {
      val dane: String = "Przykladowy tekst\nzwierajacy\ntrzy linijki"
      println(obramuj(dane))
   }
}