import org.scalatest.{FreeSpec, Matchers}

class ProjectSpec extends FreeSpec with Matchers {

  "Dragon Ball" - {
    val dejarseFajar: Movimiento = MovimientoSimple("Dejarse fajar", {_})

    val cargarKi: Movimiento = MovimientoSimple("Cargar ki", { res => res.copy(estadoAtacante = res.estadoAtacante.cargarKi)}))

    val usarSemillaHermitanio: Movimiento = UsarItem("Usar semilla del hermitaño", SemillaDelHermitanio)

    val comerseAlOponente: Movimiento = MovimientoSimple("Comerse al oponente", res => res.estadoAtacante.especie match {
      case monstruo @Monstruo(_, _) if res.estadoAtacante.energia > res.estadoOponente.energia =>
        monstruo.devorar(res.estadoAtacante, res.estadoOponente)
      case _ => res
    })

    val convertirseEnMono: Movimiento = MovimientoSimple("Convertirse en mono",  res => res.estadoAtacante.especie match {
      case saiyajin @ Saiyajin(true, false, _) => Resultado(saiyajin.transformarEnMono(res.estadoAtacante), res.estadoOponente)
      case _ => res
    })

    val transformarseEnSS: Movimiento = MovimientoSimple("Transformase en super saiyajin", res => res.estadoAtacante.especie match {
      case saiyajin @Saiyajin(_, _, _) => Resultado(saiyajin.convertiseEnSuperSaiyajin(res.estadoAtacante), res.estadoOponente)
      case _ => res
    })

    "tests" - {
      "debería resolver las dependencias y pasar este test" in {
        Prueba.materia shouldBe "tadp"
      }
    }
  }

}
