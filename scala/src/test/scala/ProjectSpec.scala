import org.scalatest.{FreeSpec, Matchers}

class ProjectSpec extends FreeSpec with Matchers {

  "Dragon Ball" - {
    val dejarseFajar: Movimiento = MovimientoSimple("Dejarse fajar", {_})

    val cargarKi: Movimiento = MovimientoSimple("Cargar ki", { res => res.copy(estadoAtacante = res.estadoAtacante.cargarKi)}))

    val usarSemillaHermitanio: Movimiento = UsarItem("Usar semilla del hermitaño", SemillaDelHermitanio)

    val comerseAlOponente: Movimiento = MovimientoSimple("Comerse al oponente", {
      case Peleando(atacante @ Monstruo(formaDeComer, _), oponente) if atacante.energia > oponente.energia =>
        Resultado(formaDeComer(atacante, oponente), oponente.morir)
      case otro => otro
    })

    val convertirseEnMono: Movimiento = MovimientoSimple("Convertirse en mono". {

    })

    "tests" - {
      "debería resolver las dependencias y pasar este test" in {
        Prueba.materia shouldBe "tadp"
      }
    }
  }

}
