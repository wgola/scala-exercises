import scala.io.Source
import akka.actor.*

case class Init(liczbaPracownikow: Int)
case class Zlecenie(tekst: List[String])
case class Wynik(liczba: Int)
case class Wykonaj(linia: String)
case object Start

class Szef extends Actor {
  def receive: Receive = {
    case Init(n) => {
      val p: List[ActorRef] = (for {
        x <- (1 to n)
      } yield context.actorOf(Props[Pracownik](), s"Pracownik${x}")).toList
      context.become(oczekiwanie(p))
    }
    case _ =>
  }
  def oczekiwanie(pracownicy: List[ActorRef]): Receive = {
    case Zlecenie(dane) => {
      context.become(wysylanie(dane, pracownicy, 0, dane.length, 0, pracownicy))
      self ! Start
    }
    case _ =>
  }
  def wysylanie(
      dane: List[String],
      pracownicy: List[ActorRef],
      wynik: Int,
      ileLinii: Int,
      ileDoszlo: Int,
      wszyscyPracownicy: List[ActorRef]
  ): Receive = {
    case Start =>
      (dane, pracownicy) match {
        case (linia :: resztaD, pracownik :: resztaP) => {
          pracownik ! Wykonaj(linia)
          context.become(wysylanie(resztaD, resztaP, wynik, ileLinii, ileDoszlo, wszyscyPracownicy))
          self ! Start
        }
        case (_, _) =>
      }
    case Wynik(n) if ileDoszlo + 1 < ileLinii =>
      dane match {
        case linia :: reszta => {
          sender() ! Wykonaj(linia)
          context.become(
            wysylanie(reszta, pracownicy, wynik + n, ileLinii, ileDoszlo + 1, wszyscyPracownicy)
          )
        }
        case _ =>
          context.become(
            wysylanie(dane, pracownicy, wynik + n, ileLinii, ileDoszlo + 1, wszyscyPracownicy)
          )
      }
    case Wynik(n) if ileDoszlo + 1 == ileLinii => {
      println(wynik + n)
      context.become(oczekiwanie(wszyscyPracownicy))
    }
    case _ =>
  }
}

class Pracownik extends Actor {
  def receive: Receive = {
    case Wykonaj(linia) => {
      sender() ! Wynik(linia.split(" ").toList.length)
    }
    case _ =>
  }
}

@main def lab12zad1: Unit = {
  val plik: List[String] = Source.fromResource("ogniem-i-mieczem.txt").getLines.toList
  val system = ActorSystem("lab12zad1")
  val zarzadca = system.actorOf(Props[Szef](), "Szef")
  zarzadca ! Init(10)
  zarzadca ! Zlecenie(plik)
}
