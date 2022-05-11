import scala.annotation.tailrec

def MasterMind(secret: List[Int], guess: List[Int]): (Int, Int) = {
    val trafione = secret.intersect(guess).size

    val secretZipped = secret.zipWithIndex
    val guessZipped = guess.zipWithIndex

    val czarne = secretZipped.intersect(guessZipped).size

    val biale = trafione - czarne

    (czarne, biale)
}

@main def lab8zad1(): Unit = {
    val secret = List(1, 3, 2, 2, 4, 5)
    val guess = List(2, 1, 2, 4, 7, 2)

    println(MasterMind(secret, guess))

}