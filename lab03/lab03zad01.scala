import scala.annotation.tailrec

@tailrec
def reverse(str: String, akum: String = ""): String = {
    if (str.length == 0) akum
    else reverse(str.tail, str.head + akum)
}


@main def lab3zad1(): Unit = {
   println(reverse("ala ma kota"))
}