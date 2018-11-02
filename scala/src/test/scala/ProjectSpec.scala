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
      case Monstruo(_,_) => EstadoResultado(res.estadoAtacante.morir, res.estadoOponente.recibirExplosion(res.estadoAtacante.energia * 2))
      case Androide => EstadoResultado(res.estadoAtacante.morir, res.estadoOponente.recibirExplosion(res.estadoAtacante.energia * 3))
      case _ => res
    })

    "movimientos" - {
      val humanoGenerico: Guerrero = Guerrero("Dummy", 1, 10, Humano)
      "dejarse fajar" - {
        val movDejarseFajar = Map[String, Movimiento]((dejarseFajar.nombre, dejarseFajar))
        "goku que sabe dejarse fajar lo hace por un turno" in {
          val goku: Guerrero = Guerrero("Goku", 100, 100, Saiyajin(), movDejarseFajar)
          dejarseFajar.ejecutar(EstadoResultado(goku, humanoGenerico)).estadoAtacante shouldBe goku.copy(turnosSiendoFajado = 1)
        }

        "goku que no sabe dejarse fajar lo hace por un turno" in {
          val goku: Guerrero = Guerrero("Goku", 100, 100, Saiyajin())
          dejarseFajar.ejecutar(EstadoResultado(goku, humanoGenerico)).estadoAtacante shouldBe goku
        }
      }

      "cargar ki" - {
        val movCargarKi = Map[String, Movimiento]((cargarKi.nombre, cargarKi))
        "yamcha cualquiera carga su ki" in {
          val yamcha: Guerrero = Guerrero("Yamcha", 100, 200, Humano, movCargarKi)

          cargarKi.ejecutar(EstadoResultado(yamcha, humanoGenerico)).estadoAtacante shouldBe yamcha.copy(energia = 200)
        }

        "goku sin ss carga su ki" in {
          val goku: Guerrero = Guerrero("Goku", 100, 400, Saiyajin(), movCargarKi)

          cargarKi.ejecutar(EstadoResultado(goku, humanoGenerico)).estadoAtacante shouldBe goku.copy(energia = 200)
        }

        "goku con nivel ss 2 carga su ki" in {
          val goku: Guerrero = Guerrero("Goku", 100, 400, Saiyajin(false, false, 2), movCargarKi)

          cargarKi.ejecutar(EstadoResultado(goku, humanoGenerico)).estadoAtacante shouldBe goku.copy(energia = 400)
        }

        "A17 carga su ki" in {
          val A17: Guerrero = Guerrero("Androide 17", 100, 200, Androide, movCargarKi)

          cargarKi.ejecutar(EstadoResultado(A17, humanoGenerico)).estadoAtacante shouldBe A17
        }

        "yamcha inconsiente no puede cargar ki" in {
          val yamcha: Guerrero = Guerrero("Yamcha", 100, 200, Humano, movCargarKi, Map[String, Item](), Inconsciente)

          cargarKi.ejecutar(EstadoResultado(yamcha, humanoGenerico)).estadoAtacante shouldBe yamcha
        }

        "krillin muerto no puede cargar ki" in {
          val krillin: Guerrero = Guerrero("Krillin", 0, 200, Humano, movCargarKi, Map[String, Item](), Muerto)

          cargarKi.ejecutar(EstadoResultado(krillin, humanoGenerico)).estadoAtacante shouldBe krillin
        }
      }

      "Usar item" - {
        "Semilla del hermitaño" - {
          val movUsarSemilla = Map[String, Movimiento]((usarSemillaHermitanio.nombre, usarSemillaHermitanio))
          val tenerSemilla = Map[String, Item]((SemillaDelHermitanio.nombre, SemillaDelHermitanio))

          "Yajirobe usa una semilla del hermitaño" in {
            val yajirobe: Guerrero = Guerrero("Yajirobe", 1, 100, Humano, movUsarSemilla, tenerSemilla)

            usarSemillaHermitanio.ejecutar(EstadoResultado(yajirobe, humanoGenerico)).estadoAtacante shouldBe yajirobe.copy(energia = yajirobe.energiaMaxima)
          }

          "Yajirobe intenta usar una semilla del hermitaño pero no le quedan" in {
            val yajirobe: Guerrero = Guerrero("Yajirobe", 1, 100, Humano, movUsarSemilla)

            usarSemillaHermitanio.ejecutar(EstadoResultado(yajirobe, humanoGenerico)).estadoAtacante shouldBe yajirobe
          }

          "Goku recibe una semilla del hermitaño inconsciente" in {
            val goku: Guerrero = Guerrero("Goku", 1, 100, Saiyajin(), movUsarSemilla, tenerSemilla, Inconsciente)

            usarSemillaHermitanio.ejecutar(EstadoResultado(goku, humanoGenerico)).estadoAtacante shouldBe goku.copy(energia = goku.energiaMaxima, estado = Normal)
          }

          "Goku muerto no puede usar una semilla del hermitaño" in {
            val goku: Guerrero = Guerrero("Goku", 0, 100, Saiyajin(), movUsarSemilla, tenerSemilla, Muerto)

            usarSemillaHermitanio.ejecutar(EstadoResultado(goku, humanoGenerico)).estadoAtacante shouldBe goku
          }
        }
        "arma roma" - {
          val palo = Arma("Palo", Roma)
          val usarPalo = UsarItem(palo)
          val movUsarPalo = Map[String, Movimiento]((usarPalo.nombre, usarPalo))
          val tienePalo = Map[String, Item]((palo.nombre, palo))
          val humanoConPalo: Guerrero = Guerrero("Humano(con palo)", 10, 10, Humano, movUsarPalo, tienePalo)

          "Humano con palo ataca a otro" in {

            usarPalo.ejecutar(EstadoResultado(humanoConPalo, humanoGenerico)).estadoOponente shouldBe humanoGenerico.copy(estado = Inconsciente)
          }

          "Humano con palo ataca a Goku" in {
            val goku: Guerrero = Guerrero("Goku", 400, 400, Saiyajin())

            usarPalo.ejecutar(EstadoResultado(humanoConPalo, goku)).estadoOponente shouldBe goku
          }

          "Humano con palo inconsciente no puede atacar a nadie" in {
            usarPalo.ejecutar(EstadoResultado(humanoConPalo.copy(estado = Inconsciente), humanoGenerico)).estadoOponente shouldBe humanoGenerico
          }

          "Humano con palo muerto no puede atacar a nadie" in {
            usarPalo.ejecutar(EstadoResultado(humanoConPalo.morir, humanoGenerico)).estadoOponente shouldBe humanoGenerico
          }

          "Humano con palo ataca a A17" in {
            val A17: Guerrero = Guerrero("Androide 17", 100, 200, Androide)

            usarPalo.ejecutar(EstadoResultado(humanoConPalo, A17)).estadoOponente shouldBe A17
          }
        }
      }
    }
  }

}
