def isOrdered[A](l: List[A])(leq: (A, A) => Boolean): Boolean = {
    def helper(lista: List[A], leq: (A, A) => Boolean, poprzedni: Option[A] = None): Boolean = {
        lista match {
            case element :: reszta => {
                    if (poprzedni.nonEmpty) {
                        if(leq(poprzedni.get, element)) helper(reszta, leq, Some(element))
                        else false
                    }
                    else helper(reszta, leq, Some(element))
                }
            case _ => true
            }
        }
        helper(l, leq)
    }
    


@main def lab5zad3(): Unit = {

    val l = List(1, 2, 2, 4)
    val leq = (a: Int, b: Int) => a <= b

    println(isOrdered(l)(leq))

}