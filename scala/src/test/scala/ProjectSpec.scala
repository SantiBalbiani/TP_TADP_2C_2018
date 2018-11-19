import org.scalatest.{FreeSpec, Matchers}
import tipos._

class ProjectSpec extends FreeSpec with Matchers {

  "DragonBall" - {


    "vegetaTieneUnaSemillaDelErmitanio" in {

      val ataque1: Movimiento = Movimiento("GolpeSuave",(g1:Guerrero, g2:Guerrero) => (g1.copy(), g2.copy(ki = g2.ki - 10)))
      var vegeta = Guerrero("vegeta",100,12, List(ataque1), Sayajin(12, false, false), List(SemillaHermitanio), Normal)

      vegeta.tieneElItem(SemillaHermitanio) shouldEqual true

    }

    "Movimiento que mas Danio hace" in {

      val ataque1: Movimiento = Movimiento("GolpeSuave",(g1:Guerrero, g2:Guerrero) => (g1.copy(), g2.copy(ki = g2.ki - 10)))
      val ataque2: Movimiento = Movimiento("GolpeMedio",(g1:Guerrero, g2:Guerrero) => (g1.copy(), g2.copy(ki = g2.ki - 20)))
      val ataque3: Movimiento = Movimiento("GolpeFuerte",(g1:Guerrero, g2:Guerrero) =>
        ( g2.tipo match {
          case Humano => (g1.copy(), g2.copy(ki = g2.ki - 60))
          case Sayajin(_,_,_) => (g1.copy(),g2.copy(ki = g2.ki - 20))
          case _ => (g1.copy(), g2.copy(ki = g2.ki - 30))
        })
      )

      var krilin = Guerrero("krilin", 100,12,List(ataque1, ataque2, ataque3), Humano, List(SemillaHermitanio), Normal)
      var vegeta = Guerrero("vegeta",100,12, List(ataque1, ataque2, ataque3), Sayajin(12, false, false), List(SemillaHermitanio), Normal)


      val movimientoQueHaceMasDanio: Movimiento = vegeta.movimientoMasEfectivoContra(krilin) (prioridadAtaque)

      movimientoQueHaceMasDanio shouldEqual ataque3
    }

    "Pelear 1 Round: estado inicial: Krilin 100ki Vegeta 100ki - final Krilin 40ki Vegeta 90ki" in {

      val ataque1: Movimiento = Movimiento("GolpeSuave",(g1:Guerrero, g2:Guerrero) => (g1.copy(), g2.copy(ki = g2.ki - 10)))
      val ataque2: Movimiento = Movimiento("GolpeMedio",(g1:Guerrero, g2:Guerrero) => (g1.copy(), g2.copy(ki = g2.ki - 20)))
      val ataque3: Movimiento = Movimiento("GolpeFuerte",(g1:Guerrero, g2:Guerrero) =>
        ( g2.tipo match {
          case Humano => (g1.copy(), g2.copy(ki = g2.ki - 60))
          case Sayajin(_,_,_) => (g1.copy(),g2.copy(ki = g2.ki - 20))
          case _ => (g1.copy(), g2.copy(ki = g2.ki - 30))
        })
      )

      var krilin = Guerrero("krilin", 100,12,List(ataque1), Humano, List(SemillaHermitanio), Normal)
      var vegeta = Guerrero("vegeta",100,12, List(ataque1,ataque2,ataque3), Sayajin(12, false, false), List(SemillaHermitanio), Normal)

      val primerRound = krilin.pelearRound(ataque1)(vegeta)

      primerRound._1.ki shouldEqual 40


    }



    }


}
