// def freqMax[A](list: List[A]): (Set[A],Int) = {
    
// }

@main def lab6zad6(): Unit = {
    val x  = List(1, 1, 2, 4, 4, 4, 1, 3)
    val a = x.map(n => (n, x.count(b => b == n)))
    val maks = x.foldLeft(0) (
        (acc, elem) => {
            val tmp = x.count(n => n == elem)
            if (tmp > acc) tmp
            else acc
        }
    )
    
}