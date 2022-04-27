def freq[A](seq: Seq[A]): Set[(A, Int)] = {
    val tmp = seq.groupBy(n => n)
    val result = tmp.foldLeft(Set[(A, Int)]())((acc, elem) => {
        val x = (elem(0), elem(1).size)
        acc + x
    })
    result
}

@main def lab7zad1(): Unit = {
    val seq = Seq('a','b','a','c','c','a')
    println(freq(seq))
}