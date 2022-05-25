def sumOpts(seq: Seq[Option[Double]]): Option[Double] = {
  val wynik = seq.foldLeft(Option[Double](0.0))((acc, elem) => {
    if (elem.nonEmpty) Option(elem.get + acc.get)
    else acc
  })
  if (wynik.get == 0.0) None
  else wynik
}

@main def lab7zad2(): Unit = {
  val lista = List(Some(5.4), Some(-2.0), Some(1.0), None, Some(2.6))
  println(sumOpts(lista) == Some(7.0))
  println(sumOpts(List()) == None)
  println(sumOpts(List(None, None)) == None)
}
