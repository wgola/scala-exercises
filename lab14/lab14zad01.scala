package jp1.akka.lab13

// „Interfejs użytkownika” wymaga pewnych dodatkowych elementów:
import scala.concurrent.ExecutionContext
import scala.util.control.Breaks._
import scala.io.StdIn

import akka.actor.{ActorSystem, Props}

import model.*

@main
def lab14zad1: Unit = {

  val system = ActorSystem("system")
  val organizator = system.actorOf(Props[Organizator](), "organizator")

  // Interfejs „organizatora”:
  implicit val ec: ExecutionContext = ExecutionContext.global

  breakable {
    while (true) {
      StdIn.readLine("polecenie: ") match {
        case "start" =>
          // początek zawodów
          organizator ! Organizator.Start
        case "eliminacje" =>
          // polecenie rozegrania rundy eliminacyjnej
        case "finał" =>
          // polecenie rozegrania rundy finałowej
          // wymaga zamknięcia Rundy eliminacyjnej i utworzenie
          // Rundy finałowej, zawierającej najlepszych 20.
          // zawodników z Rundy eliminacyjnej
        case "wyniki" =>
          // żądanie rankingów (lub rankingu finałowego)
        case "stop" =>
          organizator ! Organizator.Stop
          break()
        case _ =>
      }
    }
  }

}
