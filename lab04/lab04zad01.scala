import scala.annotation.tailrec

def sumuj(l: List[Option[Double]]): Option[Double] = {
    @tailrec
    def helper(l: List[Option[Double]], akum: Option[Double]): Option[Double] = {
        l match {
            case head :: tail => {
                if (head.getOrElse(0.0) > 0) helper(tail, Some(akum.getOrElse(0.0) + head.get))
                else helper(tail, akum) 
            }
            case _ => akum
        }
    }
    helper(l, None)
} 

@main def lab4zad1(): Unit = {
    println(sumuj(List(Some(2.0), Some(4.0), Some(-3.0), None, Some(-3.0), None, Some(1.0))))
}