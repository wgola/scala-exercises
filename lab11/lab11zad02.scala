import akka.actor.*

case object Pileczka2
case class Graj2(przeciwnik: ActorRef, maks: Int)

class Gracz2 extends Actor {
  def receive: Receive = {
    case Graj2(przeciwnik, maks) => {
      context.become(ileOdbic(0, 0, maks))
      przeciwnik ! Graj2(self, maks)
    }
  }
  def ileOdbic(ileMoich: Int, ilePrzeciwnika: Int, maks: Int): Receive = {
    case Graj2(_, _) => {
      println(s"Pileczka ${self.path.name}")
      sender() ! Pileczka2
      context.become(ileOdbic(ileMoich + 1, ilePrzeciwnika, maks))
    }
    case Pileczka2 if ileMoich + ilePrzeciwnika + 2 <= maks => {
      println(s"Pileczka ${self.path.name}")
      sender() ! Pileczka2
      context.become(ileOdbic(ileMoich + 1, ilePrzeciwnika + 1, maks))
    }
    case _ => {
      println("Koniec")
    }
  }
}

@main def lab11zad2(): Unit = {
  val system = ActorSystem("Zad2")
  val gracz1 = system.actorOf(Props[Gracz2](), "gracz1")
  val gracz2 = system.actorOf(Props[Gracz2](), "gracz2")
  gracz1 ! Graj2(gracz2, 5)
}
