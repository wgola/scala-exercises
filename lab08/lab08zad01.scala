def MasterMind(secret: List[Int], guess: List[Int]): (Int, Int) = {
    val trafione = secret.foldLeft(0)( (acc, elem) => {
        val tmp = guess.takeWhile(n => n != elem)
        if (tmp.size >= secret.size) acc
        else acc + 1
    })

    val secretZipped = secret.zipWithIndex
    val guessZipped = guess.zipWithIndex

    val test = for {
        a <- secretZipped
        b <- guessZipped if a == b
    } yield b

    val black = test.size
    val white = trafione - black

    (black, white)
}

@main def lab8zad1(): Unit = {
    val secret = List(1, 1, 5, 3, 2, 6)
    val guess = List(1, 2, 5, 2, 1, 3)

    println(MasterMind(secret, guess))
}