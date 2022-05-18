import scala.io.Source

@main def lab9zad2(): Unit = {
    val plik: List[String] = Source.fromResource("ogniem-i-mieczem.txt").getLines.toList
    val plik2 = plik.foldLeft("")( (acc, elem) => {
        val tmp = elem.toList.foldLeft("")( (acc1, elem1) => {
            if (elem1.isLetter) acc1 + elem1.toLower
            else acc1
        })
        acc + tmp
    })
    
}