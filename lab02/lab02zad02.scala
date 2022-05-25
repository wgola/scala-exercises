@annotation.tailrec
def nwd(a: Int, b: Int): Int = {
  if (a % b == 0) b
  else nwd(b, a % b)
}

@main def lab2zad2(a: Int, b: Int): Unit = {
  println("Podane argumenty: " + a + ", " + b)
  println("Największy wspólny dzielnik: " + nwd(a, b))
}
