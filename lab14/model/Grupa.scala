package jp1.akka.lab13.model

import akka.actor.{Actor, ActorRef}

object Grupa {
  case object Runda
  // Zawodnicy mają wykonać swoje próby – Grupa
  // kolejno (sekwencyjnie) informuje zawodników
  // o konieczności wykonania próby i „oczekuje”
  // na ich wynik (typu Option[Ocena])
  case object Wyniki
  // Polecenie zwrócenia aktualnego rankingu Grupy
  // Oczywiście klasyfikowani są jedynie Zawodnicy,
  // którzy pomyślnie ukończyli swoją próbę
  case class Wynik(ocena: Option[Ocena])
  // Informacja o wyniku Zawodnika (wysyłana przez Zawodnika do Grupy)
  // np. Wynik(Some(Ocena(10, 15, 14)))
  // Jeśli zawodnik nie ukończy próby zwracana jest wartość Wynik(None)
  case object Koniec
  // Grupa kończy rywalizację
}
class Grupa(zawodnicy: List[ActorRef]) extends Actor {
  def receive: Receive = {
    case Grupa.Runda => {
      zawodnicy.foreach(z => z ! Zawodnik.Próba)
      context.become(przeprowadzam(Map()))
    }
  }
  def przeprowadzam(w: Map[ActorRef, Option[Ocena]]): Receive = {
    case Grupa.Wynik(wynik) => {
      val dodane = w + (sender() -> wynik)
      context.parent ! Organizator.Wyniki(dodane)
      context.become(przeprowadzam(dodane))
    }
    case Grupa.Wyniki => {
      val udane = w.filter(n => n match { 
          case (aktor, wynik) if wynik == None => false
          case _ => true 
        })
      sender() ! Organizator.Wyniki(udane)
    }
  }
}
