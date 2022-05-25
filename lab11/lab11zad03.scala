import akka.actor.*

case object Pileczka3
case class Graj3(przeciwnik1: ActorRef, przeciwnik2: ActorRef)

class Gracz3 extends Actor {
  def receive: Receive = {
    case Graj3(actor1, actor2) => {
      context.become(zGraczem(actor1))
      actor1 ! Graj3(actor2, self)
    }
  }
  def zGraczem(actor: ActorRef): Receive = {
    case Graj3(_, _) => actor ! Pileczka3
    case Pileczka3 => println(s"Pileczka ${self.path.name}"); actor ! Pileczka3
  }
}

@main def lab11zad3(): Unit = {
  val system = ActorSystem("HaloAkka3")
  val gracz1 = system.actorOf(Props[Gracz3](), "Gracz1")
  val gracz2 = system.actorOf(Props[Gracz3](), "Gracz2")
  val gracz3 = system.actorOf(Props[Gracz3](), "Gracz3")
  gracz1 ! Graj3(gracz2, gracz3)
}
