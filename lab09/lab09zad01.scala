import scala.io.Source

@main def lab9zad1(): Unit ={
    val plik: List[String] = Source.fromResource("nazwiska.txt").getLines.toList
    val test = plik.map(n => n.split(" ").toList)
    val max = test.foldLeft(List[Int]())( (acc, elem) => acc :+ elem.head.toList.distinct.size).distinct.sortBy(identity)
    val map = test.groupBy(n => n.head.toList.distinct.size)
    val wynik = max.foldLeft(List[(Int, String)]())( (acc, n) => {
        val tmp = map(n).foldLeft(map(n)(0))( (acc1, x) => {
            if (x.last.size < acc1.last.size) x
            else acc1
        })
        acc :+ (n, tmp.head + " " + tmp.last)
    })
    wynik.foreach(n => println(n))
}
