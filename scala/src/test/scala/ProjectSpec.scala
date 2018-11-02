import org.scalatest.{FreeSpec, Matchers}

class ProjectSpec extends FreeSpec with Matchers {

  "Dragon Ball" - {
    val dejarseFajar: Movimiento = MovimientoSimple("Dejarse fajar", res => EstadoResultado(res.estadoAtacante.dejarseFajar, res.estadoOponente))

    val cargarKi: Movimiento = MovimientoSimple("Cargar ki", { res => res.copy(estadoAtacante = res.estadoAtacante.cargarKi)})

    val usarSemillaHermitanio: Movimiento = UsarItem(SemillaDelHermitanio)

    val comerseAlOponente: Movimiento = MovimientoSimple("Comerse al oponente", res => res.estadoAtacante.especie match {
      case monstruo @Monstruo(_, _) if res.estadoAtacante.energia > res.estadoOponente.energia =>
        monstruo.devorar(res.estadoAtacante, res.estadoOponente)
      case _ => res
    })

    val convertirseEnMono: Movimiento = MovimientoSimple("Convertirse en mono",  res => res.estadoAtacante.especie match {
      case saiyajin @ Saiyajin(true, false, _) => EstadoResultado(saiyajin.transformarEnMono(res.estadoAtacante), res.estadoOponente)
      case _ => res
    })

    val transformarseEnSS: Movimiento = MovimientoSimple("Transformase en super saiyajin", res => res.estadoAtacante.especie match {
      case saiyajin @Saiyajin(_, _, _) => EstadoResultado(saiyajin.convertiseEnSuperSaiyajin(res.estadoAtacante), res.estadoOponente)
      case _ => res
    })

    val MGN: Movimiento = AtaqueFisico("Muchos golpes ninja",  res =>
      (res.estadoAtacante.especie, res.estadoOponente.especie) match {
        case (Humano, Androide) => EstadoResultado(res.estadoAtacante.reducirKi(10), res.estadoOponente)
        case (_,_) => res.afectarMasDebil(_.reducirKi(20))
      })

    val explotar: Movimiento = AtaqueFisico("Explotar", res => res.estadoAtacante.especie match {
      case Monstruo(_,_) => EstadoResultado(res.estadoAtacante.morir(), res.estadoOponente.recibirExplosion(res.estadoAtacante.energia * 2))
      case Androide => EstadoResultado(res.estadoAtacante.morir(), res.estadoOponente.recibirExplosion(res.estadoAtacante.energia * 3))
      case _ => res
    })

    "movimientos" - {
      val guerreroDummy: Guerrero = Guerrero("Dummy", 100, 100, Humano)
      "dejarse fajar" in {
        val goku: Guerrero = Guerrero("Goku", 100, 100, Saiyajin(), Map[String, Movimiento]((dejarseFajar.nombre, dejarseFajar)))
        dejarseFajar.ejecutar(EstadoResultado(goku, guerreroDummy)).estadoAtacante shouldBe goku.copy(turnosSiendoFajado = 1)
      }
    }
  }

}
