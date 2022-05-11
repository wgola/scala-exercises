def threeNumbers(n: Int): Seq[(Int, Int, Int)] = {
    val numbers = (1 to n).toList

    val result = for {
        a <- numbers 
        b <- numbers if b > a
        c <- numbers if c*c == a*a + b*b
    } yield (a, b, c)
    
    result
}


@main def lab8zad3(): Unit = {
    println(threeNumbers(26))
}