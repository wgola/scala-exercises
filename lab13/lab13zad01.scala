import akka.actor.*
import scala.concurrent.duration._
import scala.util.Random
/*
  W konfiguracji projektu wykorzystana została wtyczka
  sbt-revolver. W związku z tym uruchamiamy program poleceniem

    reStart

  a zatrzymujemy pisząc (mimo przesuwających się komunikatów)

     reStop

  i naciskając klawisz ENTER. Jeśli czynności powyższe
  już wykonywaliśmy to możemy też przywołać poprzednie
  polecenia używając strzałek góra/dół na klawiaturze.
*/


object SiłaWyższa {
  case object Cyk
  case object Strzelać
}

case object StartWalki
case class Atakuj(z: ActorRef)
case object ZaAtakuj

class SiłaWyższa extends Actor {
  import SiłaWyższa._
  def receive = {
    case Cyk => {
      println("Cyk")
      val zamek1 = context.actorOf(Props[Zamek](), "Zamek1")
      val zamek2 = context.actorOf(Props[Zamek](), "Zamek2")
      zamek1 ! StartWalki
      zamek2 ! StartWalki
      context.become(rozgrywka(zamek1, zamek2))
    }
  }

  def rozgrywka(z1: ActorRef, z2: ActorRef): Receive = {
    case Cyk => {
      println("---")
      z1 ! Atakuj(z2)
      z2 ! Atakuj(z1)
    }
  }
}

class Zamek extends Actor {
  def receive: Receive = {
    case StartWalki => {
      val obrońcy = (for {
        x <- (1 to 101).toList
      } yield context.actorOf(Props[Obrońca](), s"Obronca$x")).toList
      obrońcy.foreach(n => context.watch(n))
      context.become(zObrońcami(obrońcy, 100))
    }
  }

  def zObrońcami(o: List[ActorRef], ile: Int): Receive = {
    case Atakuj(x) => {
      println(s"${self.path.name}: ${ile}")
      o.foreach(n => n ! Atakuj(x))
    }
    case Terminated(obronca) => {
      if (ile == 1) {
        println(s"${self.path.name} przegrał!")
        context.system.terminate()
      } else {
        context.become(zObrońcami(o.filter(n => n != obronca), ile - 1))
      }
    }
    case ZaAtakuj => {
      val indeks = Random.between(0, ile)
      val szansa = (((ile.toFloat)/(2*100).toFloat)*100).toInt
      val procent = Random.between(1, 101)
      if (procent <= szansa) {
        o(indeks) ! PoisonPill
      } 
    }
  }
}

class Obrońca extends Actor {
  def receive: Receive = {
    case Atakuj(x) => {
      x ! ZaAtakuj
    }
  }
}

@main
def lab13zad1: Unit = {
  val system = ActorSystem("lab13zad1")
  import system.dispatcher

  // UWAGA: „nazwy”, tworzące ścieżkę do aktora muszą być zapisywane
  // z użyciem znaków znaków ASCII (a nie np. UTF8) – stąd „SilaWyzsza”
  val siłaWyższa = system.actorOf(Props[SiłaWyższa](), "SilaWyzsza")

  // Do „animacji” SiłyWyższej wykorzystamy „Planistę” (Scheduler)
  val pantaRhei = system.scheduler.scheduleWithFixedDelay(
    Duration.Zero,     // opóźnienie początkowe
    500.milliseconds, // odstęp pomiedzy kolejnymi komunikatami
    siłaWyższa,        // adresat „korespondencji”
    SiłaWyższa.Cyk     // komunikat
  )
}
