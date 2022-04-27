@main def lab7zad6(): Unit = {
    val strefy: Seq[String] = java.util.TimeZone.getAvailableIDs.toSeq
    val europa = strefy.filter(a => a match { 
        case a if a.startsWith("Europe/") => true
        case _ => false})
    val uciete = europa.map(n => n.stripPrefix("Europe/")).toList
    val posortowane = uciete.sortBy(x => (x.length, x.head))
    println(posortowane)
}