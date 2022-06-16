package jp1.akka.lab13.model

import akka.actor.{Actor, ActorRef, Props}
import scala.concurrent.java8.FuturesConvertersImpl.P
import akka.actor.PoisonPill

val akkaPathAllowedChars = ('a' to 'z').toSet union
  ('A' to 'Z').toSet union
  "-_.*$+:@&=,!~';.)".toSet

object Organizator {
  case object Start
  // rozpoczynamy zawody – losujemy 50 osób, tworzymy z nich zawodników
  // i grupę eliminacyjną
  case object Runda
  // polecenie rozegrania rundy (kwalifikacyjnej bądź finałowej) –  wysyłamy Grupa.Runda
  // do aktualnej grupy
  case object Wyniki
  // polecenie wyświetlenia klasyfikacji dla aktualnej grupy
  case class Wyniki(w: Map[ActorRef, Option[Ocena]])
  // wyniki zwracane przez Grupę
  case object Stop
  // kończymy działanie
}

class Organizator extends Actor {
  // importujemy komunikaty na które ma reagować Organizator
  import Organizator._

  def receive: Receive = {
    case Start => {
      // tworzenie 50. osób, opdowiadających im Zawodników
      // oraz Grupy eliminacyjnej
      val zawodnicy = List.fill(50) {
        val o = Utl.osoba()
        context.actorOf(Props(Zawodnik(o)), s"${o.imie}-${o.nazwisko}" filter akkaPathAllowedChars)
      }
      val grupa_eliminacyjna = context.actorOf(Props(Grupa(zawodnicy)), "grupa_el")
      context.become(przeprowadzaneEliminacje(grupa_eliminacyjna, 0))

    // Obsługa pozostałych komunikatów
      }
    case Stop =>
      println("kończymy zawody...")
      context.system.terminate()
  }

  def przeprowadzaneEliminacje(g: ActorRef, ile: Int): Receive = {
    case Runda => {
      g ! Grupa.Runda
    } 
    case Wyniki(_) => {
      if (ile + 1 == 50) {
        println("Eliminacje zakończone!")
        context.become(poEliminacjach(g))
        self ! Runda
      } else {
        context.become(przeprowadzaneEliminacje(g, ile + 1))
      }
    }
  }

  def poEliminacjach(g: ActorRef): Receive = {
    case Runda => {
      g ! Grupa.Wyniki
    }
    case Wyniki(wyniki_eliminacje) => 
      g ! PoisonPill
      context.become(zWynikamiPoEliminacjach(wyniki_eliminacje))
  }

  def zWynikamiPoEliminacjach(wyniki: Map[ActorRef, Option[Ocena]]): Receive = {
    case Runda => {
      val finalisci_wyniki = wyniki.toList.sortWith((a, b) => (a, b) match {
        case ((aktor1, Some(Ocena(nota1_1, nota2_1, nota3_1))), (aktor2, Some(Ocena(nota1_2, nota2_2, nota3_2)))) => {
          val suma1 = nota1_1 + nota2_1 + nota3_1
          val suma2 = nota1_2 + nota2_2 + nota3_2
          if (suma1 > suma2) true
          else if ((suma1 == suma2) && (nota1_1 > nota1_2)) true
          else if ((suma1 == suma2) && (nota1_1 == nota1_2) && (nota2_1 > nota2_2)) true
          else if ((suma1 == suma2) && (nota1_1 == nota1_2) && (nota2_1 == nota2_2) && (nota3_1 > nota3_2)) true
          else false 
      }}).take(20)
      val grupa_finalowa = context.actorOf(Props(Grupa(finalisci_wyniki.map((a, b) => a).toList)), "grupa_f")
      grupa_finalowa ! Grupa.Runda
      context.become(przeprowadzamFinal(grupa_finalowa, finalisci_wyniki.toMap, 0))
    }
    case Wyniki => {
      val zmapowane = wyniki.map(n => n match {
        case (zawodnik, Some(Ocena(nota1, nota2, nota3))) => (zawodnik.path.name, Ocena(nota1, nota2, nota3))
      })
      val posortowane = zmapowane.toList.sortWith((a, b) => (a, b) match {
        case ((aktor1, Ocena(nota1_1, nota2_1, nota3_1)), (aktor2, Ocena(nota1_2, nota2_2, nota3_2))) => {
          val suma1 = nota1_1 + nota2_1 + nota3_1
          val suma2 = nota1_2 + nota2_2 + nota3_2
          if (suma1 > suma2) true
          else if ((suma1 == suma2) && (nota1_1 > nota1_2)) true
          else if ((suma1 == suma2) && (nota1_1 == nota1_2) && (nota2_1 > nota2_2)) true
          else if ((suma1 == suma2) && (nota1_1 == nota1_2) && (nota2_1 == nota2_2) && (nota3_1 > nota3_2)) true
          else false 
      }})
      val ranking = posortowane.foldLeft(List[(String, Ocena, Int, Int)]())((acc, elem) => {
        if (acc.isEmpty) {
          acc :+ (elem(0), elem(1), 1, 1)
        } else {
          val poprzedni = acc.last
          (poprzedni(1), elem(1)) match {
            case (Ocena(nota11, nota21, nota31), Ocena(nota12, nota22, nota32)) => {
              if (nota11 == nota12 && nota21 == nota22 && nota31 == nota32) {
                acc :+ (elem(0), elem(1), poprzedni(2), poprzedni(3) + 1)
              } else {
                acc :+ (elem(0), elem(1), poprzedni(2) + poprzedni(3), 1)
              }
            }
          }
        }
      }).map(n => n match { case (s"${imie}-${nazwisko}", Ocena(nota1, nota2, nota3), miejsce, _) => s"${miejsce}. ${imie} ${nazwisko} - ${nota1}-${nota2}-${nota3} = ${nota1 + nota2 + nota3}"})
      println("Ranking: ")
      ranking.foreach(n => println(n))
    }
    case Stop =>
      println("kończymy zawody...")
      context.system.terminate()
  }

  def przeprowadzamFinal(g: ActorRef, fw:  Map[ActorRef, Option[Ocena]], ile: Int): Receive = {
    case Wyniki(_)=> {
      if (ile + 1 == 20) {
        println("Finał zakończony!")
        context.become(poFinale(g, fw))
      } else {
        context.become(przeprowadzamFinal(g, fw, ile + 1))
      }
    } 
  }

  def poFinale(g: ActorRef, fw: Map[ActorRef, Option[Ocena]]): Receive = {
    case Wyniki => {
      g ! Grupa.Wyniki
    }
    case Wyniki(wyniki_final) => {
      g ! PoisonPill
      val ostateczne = fw.map(n => n match {
        case (zawodnik, Some(Ocena(nota1, nota2, nota3))) => {
          if (wyniki_final.contains(zawodnik)) {
            val ocena_po_finale = wyniki_final(zawodnik).getOrElse(Ocena(0, 0, 0))
            ocena_po_finale match {
              case Ocena(nota4, nota5, nota6) => (zawodnik.path.name, Ocena(nota1 + nota4, nota2 + nota5, nota3 + nota6))
            }
          } else (zawodnik.path.name, Ocena(nota1, nota2, nota3))
        }
      })
      val ostateczne_posortowane = ostateczne.toList.sortWith((a, b) => (a, b) match {
        case ((aktor1, Ocena(nota1_1, nota2_1, nota3_1)), (aktor2, Ocena(nota1_2, nota2_2, nota3_2))) => {
          val suma1 = nota1_1 + nota2_1 + nota3_1
          val suma2 = nota1_2 + nota2_2 + nota3_2
          if (suma1 > suma2) true
          else if ((suma1 == suma2) && (nota1_1 > nota1_2)) true
          else if ((suma1 == suma2) && (nota1_1 == nota1_2) && (nota2_1 > nota2_2)) true
          else if ((suma1 == suma2) && (nota1_1 == nota1_2) && (nota2_1 == nota2_2) && (nota3_1 > nota3_2)) true
          else false 
      }})
      val ranking = ostateczne_posortowane.foldLeft(List[(String, Ocena, Int, Int)]())((acc, elem) => {
        if (acc.isEmpty) {
          acc :+ (elem(0), elem(1), 1, 1)
        } else {
          val poprzedni = acc.last
          (poprzedni(1), elem(1)) match {
            case (Ocena(nota11, nota21, nota31), Ocena(nota12, nota22, nota32)) => {
              if (nota11 == nota12 && nota21 == nota22 && nota31 == nota32) {
                acc :+ (elem(0), elem(1), poprzedni(2), poprzedni(3) + 1)
              } else {
                acc :+ (elem(0), elem(1), poprzedni(2) + poprzedni(3), 1)
              }
            }
          }
        }
      }).map(n => n match { case (s"${imie}-${nazwisko}", Ocena(nota1, nota2, nota3), miejsce, _) => s"${miejsce}. ${imie} ${nazwisko} - ${nota1}-${nota2}-${nota3} = ${nota1 + nota2 + nota3}"})
      println("Ranking: ")
      ranking.foreach(n => println(n))
      context.become(receive)
    }
  } 
}
