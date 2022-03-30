import scala.annotation.tailrec

def maksimum(l1: List[Double], l2: List[Double]): List[Double] = {
    @tailrec
    def helper(lista1: List[Double], lista2: List[Double], wynik: List[Double]): List[Double] = {
        (lista1, lista2) match {
            case (head1 :: tail1, head2 :: tail2) => {
                if (head1 > head2) helper(tail1, tail2, wynik :+ head1)
                else helper(tail1, tail2, wynik :+ head2)
            }
            case (List(), head2 :: tail2) => helper(lista1, tail2, wynik :+ head2)
            case (head1 :: tail1, List()) => helper(tail1, lista2, wynik :+ head1)
            case (_, _) => wynik 
        }
    }
    helper(l1, l2, List())
}

@main def lab4zad2(): Unit = {
    println(maksimum(List(2.0, -1.6, 3.2, 5.4, -8.4), List(3.3, -3.1, 3.2, -4.1, -0.4, 5.5)))
}