type Pred[A] = A => Boolean

def and[A](p: Pred[A], q: Pred[A]): Pred[A] = {
    n => p(n) && q(n)
}

def or[A](p: Pred[A], q: Pred[A]): Pred[A] = {
    n => p(n) || q(n)
}

def not[A](p: Pred[A]): Pred[A] = {
    n => !p(n)
}

def imp[A](p: Pred[A], q: Pred[A]): Pred[A] = {
    n => {
        if (p(n) && q(n) == false) false
        else true
    }
}

@main def lab4zad5(): Unit = {
    val wiecejniz5 = (n: Int) => n > 5
    val mniejniz9 = (n: Int) => n < 9

    val And = and(wiecejniz5, mniejniz9)
    val Or = or(wiecejniz5, mniejniz9)
    val Not = not(wiecejniz5)
    val Imp = imp(wiecejniz5, mniejniz9)

    println(And(5))
}