import akka.actor.*

case object Pileczka4
case class Graj4(przeciwnicy: List[ActorRef])

class Gracz4 extends Actor {
  def receive: Receive = {
    case Graj4(aktor1 :: aktor2 :: reszta) => {
      context.become(zGraczem(aktor2))
      aktor2 ! Graj4(aktor2 :: (reszta :+ self))
    }
  }
  def zGraczem(przeciwnik: ActorRef): Receive = {
    case Graj4(_) => przeciwnik ! Pileczka4
    case Pileczka4 => {
      println(s"${self.path.name} -> ${przeciwnik.path.name}")
      przeciwnik ! Pileczka4
    }
  }
}

@main def lab11zad4(): Unit = {
  val system = ActorSystem("zad4")
  val gracze = for {
    x <- (1 to 10).toList
  } yield system.actorOf(Props[Gracz4](), s"Gracz${x}")
  gracze.head ! Graj4(gracze)
}
