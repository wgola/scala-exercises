import akka.actor.*

case object Pileczka1
case class Graj1(przeciwnik: ActorRef)

class Gracz1 extends Actor {
  def receive: Receive = {
    case Graj1(actor) => actor ! Pileczka1
    case Pileczka1 => println(s"Pileczka ${self.path.name}"); sender() ! Pileczka1
  }
}

@main def lab11zad1(): Unit = {
  val system = ActorSystem("HaloAkka1")
  val gracz1 = system.actorOf(Props[Gracz1](), "Gracz1")
  val gracz2 = system.actorOf(Props[Gracz1](), "Gracz2")
  gracz1 ! Graj1(gracz2)
}
