import org.scalatest.{FreeSpec, Matchers}
import tipos._

class ProjectSpec extends FreeSpec with Matchers {

  "DragonBall" - {
    val ataque1: Movimiento = Movimiento("GolpeSuave", (g1: Guerrero, g2: Guerrero) => (g1.copy(), g2.copy(ki = g2.ki - 10)))
    val ataque2: Movimiento = Movimiento("GolpeMedio", (g1: Guerrero, g2: Guerrero) => (g1.copy(), g2.copy(ki = g2.ki - 20)))
    val ataque3: Movimiento = Movimiento("GolpeFuerte", (g1: Guerrero, g2: Guerrero) =>
      (g2.tipo match {
        case Humano => (g1.copy(), g2.copy(ki = g2.ki - 60))
        case Sayajin(_, _, _) => (g1.copy(), g2.copy(ki = g2.ki - 20))
        case _ => (g1.copy(), g2.copy(ki = g2.ki - 30))
      })
    )

    def usarItem(unItem:Item): Movimiento = Movimiento("usarItem", (g1: Guerrero, g2: Guerrero) =>
      if (g1.tieneElItem(unItem)){ unItem match {
        case Arma(Roma) if !g2.tipo.equals(Androide) => (g1.copy(), g2.copy(estado = Inconsciente))
        case Arma(Filosa) =>

          g2 match {
            case Guerrero(_, _, _, _, Sayajin(lvl, true, true), _, _) => (g1.copy(), g2.copy(ki = 1, tipo = Sayajin(lvl, false, false), estado = Inconsciente))
            case Guerrero(_, _, _, _, Sayajin(lvl, true, false), _, _) => (g1.copy(), g2.copy(ki = 1, tipo = Sayajin(lvl, false, false)))
            case _ => (g1.copy(), g2.copy(ki = g2.ki - (g1.ki / 100)))
          }
        case Arma(Fuego(muni)) if muni > 0 =>
          g2 match {
            case Guerrero(_, _, _, _, Humano, _, _) => (g1.copy(), g2.copy(ki = g2.ki - 20)) // Actualizar muni en g1
            case Guerrero(_, _, _, _, Namekusein, _, Inconsciente) => (g1.copy(), g2.copy(ki = g2.ki - 10)) //Igual q arriba
            case _ => (g1.copy(), g2.copy())
          }
        case SemillaHermitanio => (g1.copy(ki = g1.kiMax), g2.copy())

        case _ => (g1.copy(), g2.copy())
      }} else {
        (g1.copy(), g2.copy())
      })

    "vegetaTieneUnaSemillaDelErmitanio" in {

      val ataque1: Movimiento = Movimiento("GolpeSuave", (g1: Guerrero, g2: Guerrero) => (g1.copy(), g2.copy(ki = g2.ki - 10)))
      var vegeta = Guerrero("vegeta", 100, 12, List(ataque1), Sayajin(12, false, false), List(SemillaHermitanio), Normal)

      vegeta.tieneElItem(SemillaHermitanio) shouldEqual true

    }

    "Movimiento que mas Danio hace" in {

      var krilin = Guerrero("krilin", 100, 12, List(ataque1, ataque2, ataque3), Humano, List(SemillaHermitanio), Normal)
      var vegeta = Guerrero("vegeta", 100, 12, List(ataque1, ataque2, ataque3), Sayajin(12, false, false), List(SemillaHermitanio), Normal)


      val movimientoQueHaceMasDanio: Movimiento = vegeta.movimientoMasEfectivoContra(krilin)(prioridadAtaque)

      movimientoQueHaceMasDanio shouldEqual ataque3
    }

    "Pelear 1 Round: estado inicial: Krilin 100ki Vegeta 100ki - final Krilin 40ki Vegeta 90ki" in {

      var krilin = Guerrero("krilin", 100, 12, List(ataque1), Humano, List(SemillaHermitanio), Normal)
      var vegeta = Guerrero("vegeta", 100, 12, List(ataque1, ataque2, ataque3), Sayajin(12, false, false), List(SemillaHermitanio), Normal)

      val primerRound = krilin.pelearRound(ataque1)(vegeta)
      // Probar despues krilin.planDeAtaqueContra(vegeta, 4)(prioridadAtaque)
      // Probar movimientos complejos como usar item
      (primerRound._1.ki, primerRound._2.ki) shouldEqual(40, 90)

    }
    "Vegeta come una semilla del hermitaño pero krilin le hace 10 de daño" in {


      var krilin2 = Guerrero("krilin", 100, 120, List(ataque1), Humano, List(SemillaHermitanio), Normal)
      var vegeta2 = Guerrero("vegeta", 100, 1200, List(usarItem(SemillaHermitanio)), Sayajin(12, false, false), List(SemillaHermitanio), Normal)


      //var superVegeta = usarItem(SemillaHermitanio).ejecutarMov(vegeta2, krilin2)

      var superVegeta = vegeta2.pelearRound(usarItem(SemillaHermitanio))(krilin2)

      superVegeta._1.ki shouldEqual 1190

    }

  }
}
