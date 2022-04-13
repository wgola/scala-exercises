def pairPosNeg(seq: Seq[Double]):(Seq[Double], Seq[Double]) = {
    val (result1, result2) = seq.partition(n => n < 0.0)
    val (result3, result4) = result2.partition(n => n > 0.0)
    (result1, result3)
}

@main def lab6zad2(): Unit = {
    println(pairPosNeg(Seq(-1, 2, -3, 4, -5, 0)))
}