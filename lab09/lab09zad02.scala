import scala.io.Source

def histogram(max: Int): String = {
    val plik: List[String] = Source.fromResource("ogniem-i-mieczem.txt").getLines.toList
    val plik2 = plik.foldLeft("")( (acc, elem) => {
    val tmp = elem.toList.foldLeft("")( (acc1, elem1) => {
        if (elem1.isLetter) acc1 + elem1.toLower
        else acc1
    })
    acc + tmp
    }).toList
    val plik3 = plik2.groupMapReduce(n => n)(n => 1)((acc, elem) => acc + elem)
    val plik4 = plik3.toList.sortBy((a, b) => a)
    val maks = plik4.foldLeft(0)((acc, elem) => {
        if acc < elem(1) then elem(1)
        else acc
    })
    val wynik = plik4.foldLeft("")((acc, elem) => {
        val tmp = ((elem(1).toFloat / maks.toFloat) * (max-1)).toInt + 1
        val gwiazdki = "*" * tmp
        acc + s"${elem(0)}: ${gwiazdki}\n"
    })
    wynik
}

@main def lab9zad2(): Unit = {
    println(histogram(40))
}