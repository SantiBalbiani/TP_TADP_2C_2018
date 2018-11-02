import org.scalatest.{FreeSpec, Matchers}

class ProjectSpec extends FreeSpec with Matchers {

  "Dragon Ball" - {
    val dejarseFajar: Movimiento = MovimientoSimple("Dejarse fajar", res => EstadoResultado(res.estadoAtacante.dejarseFajar, res.estadoOponente))

    val cargarKi: Movimiento = MovimientoSimple("Cargar ki", { res => res.copy(estadoAtacante = res.estadoAtacante.cargarKi)})

    val usarSemillaHermitanio: Movimiento = UsarItem(SemillaDelHermitanio)

    val comerseAlOponente: Movimiento = MovimientoSimple("Comerse al oponente", res => res.estadoAtacante.especie match {
      case monstruo @Monstruo(_, _, _) => monstruo.devorar(res.estadoAtacante, res.estadoOponente)
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
      case Monstruo(_, _,_) => EstadoResultado(res.estadoAtacante.morir, res.estadoOponente.recibirExplosion(res.estadoAtacante.energia * 2))
      case Androide => EstadoResultado(res.estadoAtacante.morir, res.estadoOponente.recibirExplosion(res.estadoAtacante.energia * 3))
      case _ => res
    })

    "movimientos" - {
      val humanoGenerico: Guerrero = Guerrero("Dummy", 50, 50, Humano)
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

        "arma filosa" - {
          val espada = Arma("Espada", Filosa)
          val usarEspada = UsarItem(espada)
          val movUsarEspada = Map[String, Movimiento]((usarEspada.nombre, usarEspada))
          val tieneEspada = Map[String, Item]((espada.nombre, espada))
          val yajirobe: Guerrero = Guerrero("Yajirobe", 5, 5, Humano, movUsarEspada, tieneEspada)

          "yajirobe ataca a un namekusein fuerte" in {
            val namekuseinFuerte: Guerrero = Guerrero("N. Fuerte", 510, 510, Namekusein)
            usarEspada.ejecutar(EstadoResultado(yajirobe, namekuseinFuerte)).estadoOponente shouldBe namekuseinFuerte.copy(energia = 10)
          }

          "yajirobe ataca a un namekusein debil" in {
            val namekuseinDebil: Guerrero = Guerrero("N. Debil", 400, 510, Namekusein)
            usarEspada.ejecutar(EstadoResultado(yajirobe, namekuseinDebil)).estadoOponente shouldBe namekuseinDebil.morir
          }

          "yajirobe ataca goku (saiyajin sin cola)" in {
            val goku: Guerrero = Guerrero("Goku", 510, 510, Saiyajin(false))
            usarEspada.ejecutar(EstadoResultado(yajirobe, goku)).estadoOponente shouldBe goku.copy(energia = 10)
          }

          "yajirobe ataca a nappa (saiyajin con cola)" in {
            val nappa: Guerrero = Guerrero("Nappa", 510, 510, Saiyajin(true))
            usarEspada.ejecutar(EstadoResultado(yajirobe, nappa)).estadoOponente shouldBe nappa.copy(especie = Saiyajin(false), energia = 1)
          }

          "yajirobe ataca a vegeta (saiyajin hecho mono)" in {
            val vegeta: Guerrero = Guerrero("Vegeta (Mono)", 510, 510, Saiyajin(true, true))
            usarEspada.ejecutar(EstadoResultado(yajirobe, vegeta)).estadoOponente shouldBe vegeta.copy(energia = 1, especie = Saiyajin(false), estado = Inconsciente)
          }
        }

        "arma de fuego" - {
          val pistolaCargada = Arma("Pistola", DeFuego(10))
          val pistolaVacia = Arma("Pistola", DeFuego(0))
          val usarPistola = UsarItem(pistolaCargada)
          val movUsarPistola = Map[String, Movimiento]((usarPistola.nombre, usarPistola))
          val tienePistolaCargada = Map[String, Item]((pistolaCargada.nombre, pistolaCargada))
          val tienePistolaVacia = Map[String, Item]((pistolaVacia.nombre, pistolaVacia))
          val ladron: Guerrero = Guerrero("Ladron", 5, 5, Humano, movUsarPistola) // Darle pistola cargada o vacia segun test

          "ladron pierde municion post disparar" in {
            usarPistola.ejecutar(EstadoResultado(ladron.copy(inventario = tienePistolaCargada), humanoGenerico)).estadoAtacante.inventario shouldBe
              Map[String, Item]((pistolaCargada.nombre, pistolaCargada.copy(tipoArma = DeFuego(9))))
          }

          "ladron con municion dispara a un humano generico" in {
            usarPistola.ejecutar(EstadoResultado(ladron.copy(inventario = tienePistolaCargada), humanoGenerico)).estadoOponente shouldBe humanoGenerico.reducirKi(20)
          }

          "ladron sin municion dispara a un humano generico" in {
            usarPistola.ejecutar(EstadoResultado(ladron.copy(inventario = tienePistolaVacia), humanoGenerico)).estadoOponente shouldBe humanoGenerico
          }

          "ladron dispara a Goku" in {
            val goku: Guerrero = Guerrero("Goku", 100, 100, Saiyajin())

            usarPistola.ejecutar(EstadoResultado(ladron.copy(inventario = tienePistolaCargada), goku)).estadoOponente shouldBe goku
          }

          "ladron dispara a piccolo" in {
            val piccolo: Guerrero = Guerrero("Piccolo", 100, 100, Namekusein)

            usarPistola.ejecutar(EstadoResultado(ladron.copy(inventario = tienePistolaCargada), piccolo)).estadoOponente shouldBe piccolo
          }

          "ladron dispara a piccolo (inconsciente)" in {
            val piccolo: Guerrero = Guerrero("Piccolo", 100, 100, Namekusein, estado = Inconsciente)

            usarPistola.ejecutar(EstadoResultado(ladron.copy(inventario = tienePistolaCargada), piccolo)).estadoOponente shouldBe piccolo.reducirKi(10)
          }
        }
      }

      "Comerse al oponente" - {
        val movComerOponente = Map[String, Movimiento]((comerseAlOponente.nombre, comerseAlOponente))

        "Cell se come a A17" in {
          val formaDeComerDeCell: (Guerrero, Map[String, Movimiento]) => Map[String, Movimiento] = (oponente, movs) => movs ++ oponente.listarMovimientos
          val cell: Guerrero = Guerrero("Cell", 110, 110, Monstruo(_.especie == Androide, formaDeComerDeCell), movComerOponente)
          val A17: Guerrero = Guerrero("Androide 17", 100, 100, Androide, Map[String, Movimiento]((dejarseFajar.nombre, dejarseFajar)))

          val postComida: EstadoResultado = comerseAlOponente.ejecutar(EstadoResultado(cell, A17))

          postComida.estadoAtacante.listarMovimientos shouldBe
            Map[String,Movimiento]((comerseAlOponente.nombre, comerseAlOponente), (dejarseFajar.nombre, dejarseFajar))
          postComida.estadoOponente.estado shouldBe Muerto
        }

        "Cell se come a A17 y despues a A18" in {
          val formaDeComerDeCell: (Guerrero, Map[String, Movimiento]) => Map[String, Movimiento] = (oponente, movs) => movs ++ oponente.listarMovimientos
          val cell: Guerrero = Guerrero("Cell", 110, 110, Monstruo(_.especie == Androide, formaDeComerDeCell), movComerOponente)
          val A17: Guerrero = Guerrero("Androide 17", 100, 100, Androide, Map[String, Movimiento]((dejarseFajar.nombre, dejarseFajar)))
          val A18: Guerrero = Guerrero("Androide 18", 100, 100, Androide, Map[String, Movimiento]((usarSemillaHermitanio.nombre, usarSemillaHermitanio)))

          val cellPostPrimeraComida: Guerrero = comerseAlOponente.ejecutar(EstadoResultado(cell, A17)).estadoAtacante
          val postSegundaComida: EstadoResultado = comerseAlOponente.ejecutar(EstadoResultado(cellPostPrimeraComida, A18))

          postSegundaComida.estadoAtacante.listarMovimientos shouldBe
            Map[String,Movimiento]((comerseAlOponente.nombre, comerseAlOponente), (dejarseFajar.nombre, dejarseFajar),
              (usarSemillaHermitanio.nombre, usarSemillaHermitanio))
        }

        "Cell trata de comer a Goku" in {
          val formaDeComerDeCell: (Guerrero, Map[String, Movimiento]) => Map[String, Movimiento] = (oponente, movs) => movs ++ oponente.listarMovimientos
          val cell: Guerrero = Guerrero("Cell", 110, 110, Monstruo(_.especie == Androide, formaDeComerDeCell), movComerOponente)
          val goku: Guerrero = Guerrero("Goku", 100, 100, Saiyajin(), Map[String, Movimiento]((usarSemillaHermitanio.nombre, usarSemillaHermitanio)))

          val postComida:EstadoResultado = comerseAlOponente.ejecutar(EstadoResultado(cell, goku))

          postComida.estadoAtacante.listarMovimientos shouldBe cell.listarMovimientos
          postComida.estadoOponente.estado shouldBe Normal
        }

        "Goku trata de comer a Cell" in {
          val formaDeComerDeCell: (Guerrero, Map[String, Movimiento]) => Map[String, Movimiento] = (oponente, movs) => movs ++ oponente.listarMovimientos
          val cell: Guerrero = Guerrero("Cell", 110, 110, Monstruo(_.especie == Androide, formaDeComerDeCell), movComerOponente ++ Map[String, Movimiento]((usarSemillaHermitanio.nombre, usarSemillaHermitanio)))
          val goku: Guerrero = Guerrero("Goku", 100, 100, Saiyajin(), movComerOponente)

          val postComida:EstadoResultado = comerseAlOponente.ejecutar(EstadoResultado(goku, cell))

          postComida.estadoAtacante.listarMovimientos shouldBe goku.listarMovimientos
          postComida.estadoOponente.estado shouldBe Normal
        }

        "Cell trata de comer a un adroide más fuerte" in {
          val formaDeComerDeCell: (Guerrero, Map[String, Movimiento]) => Map[String, Movimiento] = (oponente, movs) => movs ++ oponente.listarMovimientos
          val cell: Guerrero = Guerrero("Cell", 110, 110, Monstruo(_.especie == Androide, formaDeComerDeCell), movComerOponente)
          val superA17: Guerrero = Guerrero("Super Androide 17", 200, 200, Androide, Map[String, Movimiento]((dejarseFajar.nombre, dejarseFajar)))

          val postComida: EstadoResultado = comerseAlOponente.ejecutar(EstadoResultado(cell, superA17))

          postComida.estadoAtacante.listarMovimientos shouldBe cell.listarMovimientos
          postComida.estadoOponente.estado shouldBe Normal
        }

        "Majin Buu se come a piccolo y gohan" in {
          val majinBuu: Guerrero = Guerrero("Majin Buu", 110, 110, Monstruo(_=>true, (op, _) => op.listarMovimientos), movComerOponente)
          val piccolo: Guerrero = Guerrero("Piccolo", 100, 100, Namekusein, Map[String, Movimiento]((cargarKi.nombre, cargarKi)))
          val gohan: Guerrero = Guerrero("Gohan", 100, 100, Saiyajin())

          val postPrimeraComida: EstadoResultado = comerseAlOponente.ejecutar(EstadoResultado(majinBuu, piccolo))
          val postSegundaComida: EstadoResultado = comerseAlOponente.ejecutar(postPrimeraComida.copy(estadoOponente = gohan))

          postPrimeraComida.estadoAtacante.listarMovimientos shouldBe majinBuu.movimientos ++ piccolo.movimientos
          postPrimeraComida.estadoOponente.estado shouldBe Muerto
          postSegundaComida.estadoAtacante.listarMovimientos shouldBe majinBuu.movimientos ++ gohan.movimientos
          postSegundaComida.estadoOponente.estado shouldBe Muerto
        }
      }

      "transformarse en mono" - {
        
      }
    }
  }

}
