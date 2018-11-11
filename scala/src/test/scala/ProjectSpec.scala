import org.scalatest.{FreeSpec, Matchers}
import tipos.Criterio

class ProjectSpec extends FreeSpec with Matchers {

  "Dragon Ball" - {
    val dejarseFajar: Movimiento = MovimientoSimple("Dejarse fajar", res => EstadoResultado(res.estadoAtacante.dejarseFajar, res.estadoOponente))

    val cargarKi: Movimiento = MovimientoSimple("Cargar ki", { res => res.copy(estadoAtacante = res.estadoAtacante.hacerAlgo(efectos.cargarKi))})

    val usarSemillaHermitanio: Movimiento = UsarItem(SemillaDelHermitanio)

    val comerseAlOponente: Movimiento = MovimientoSimple("Comerse al oponente", res => res.estadoAtacante.especie match {
      case monstruo @Monstruo(_, _, _) => monstruo.devorar(res.estadoAtacante, res.estadoOponente)
      case _ => res
    })

    val convertirseEnMono: Movimiento = MovimientoSimple("Convertirse en mono",  res => EstadoResultado(efectos.transformarEnMono(res.estadoAtacante), res.estadoOponente))

    val transformarseEnSS: Movimiento = MovimientoSimple("Transformase en super saiyajin", res => EstadoResultado(efectos.convertiseEnSuperSaiyajin(res.estadoAtacante), res.estadoOponente))

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

    val kamehameha: Movimiento = Onda("Kamehameha", 20)

    val espada = Arma("Espada", Filosa)
    val usarEspada = UsarItem(espada)

    val humanoGenerico: Guerrero = Guerrero("Dummy", 50, 50, Humano)

    "movimientos" - {
      "dejarse fajar" - {
        val movDejarseFajar = Set[Movimiento](dejarseFajar)
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
        val movCargarKi = Set[Movimiento](cargarKi)
        "yamcha cualquiera carga su ki" in {
          val yamcha: Guerrero = Guerrero("Yamcha", 100, 200, Humano, movCargarKi)

          cargarKi.ejecutar(EstadoResultado(yamcha, humanoGenerico)).estadoAtacante shouldBe yamcha.copy(energia = 200)
        }

        "goku sin ss carga su ki" in {
          val goku: Guerrero = Guerrero("Goku", 100, 400, Saiyajin(), movCargarKi)

          cargarKi.ejecutar(EstadoResultado(goku, humanoGenerico)).estadoAtacante shouldBe goku.copy(energia = 200)
        }

        "goku con nivel ss 2 carga su ki" in {
          val goku: Guerrero = Guerrero("Goku", 100, 400, Saiyajin(false, SuperSaiyajin(40, 2)), movCargarKi)

          cargarKi.ejecutar(EstadoResultado(goku, humanoGenerico)).estadoAtacante shouldBe goku.copy(energia = 400)
        }

        "A17 carga su ki" in {
          val A17: Guerrero = Guerrero("Androide 17", 100, 200, Androide, movCargarKi)

          cargarKi.ejecutar(EstadoResultado(A17, humanoGenerico)).estadoAtacante shouldBe A17
        }

        "yamcha inconsiente no puede cargar ki" in {
          val yamcha: Guerrero = Guerrero("Yamcha", 100, 200, Humano, movCargarKi, estado = Inconsciente)

          cargarKi.ejecutar(EstadoResultado(yamcha, humanoGenerico)).estadoAtacante shouldBe yamcha
        }

        "krillin muerto no puede cargar ki" in {
          val krillin: Guerrero = Guerrero("Krillin", 0, 200, Humano, movCargarKi, estado = Muerto)

          cargarKi.ejecutar(EstadoResultado(krillin, humanoGenerico)).estadoAtacante shouldBe krillin
        }
      }

      "Usar item" - {
        "Semilla del hermitaño" - {
          val movUsarSemilla = Set[Movimiento](usarSemillaHermitanio)
          val tenerSemilla = Set[Item](SemillaDelHermitanio)

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
          val movUsarPalo = Set[Movimiento](usarPalo)
          val tienePalo = Set[Item](palo)
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
          val movUsarEspada = Set[Movimiento](usarEspada)
          val tieneEspada = Set[Item](espada)
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
            val vegeta: Guerrero = Guerrero("Vegeta (Mono)", 510, 510, Saiyajin(true, Mono))
            usarEspada.ejecutar(EstadoResultado(yajirobe, vegeta)).estadoOponente shouldBe vegeta.copy(energia = 1, especie = Saiyajin(false), estado = Inconsciente)
          }
        }

        "arma de fuego" - {
          val pistolaCargada = Arma("Pistola", DeFuego(10))
          val pistolaVacia = Arma("Pistola", DeFuego(0))
          val usarPistola = UsarItem(pistolaCargada)
          val movUsarPistola = Set[Movimiento](usarPistola)
          val tienePistolaCargada = Set[Item](pistolaCargada)
          val tienePistolaVacia = Set[Item](pistolaVacia)
          val ladron: Guerrero = Guerrero("Ladron", 5, 5, Humano, movUsarPistola) // Darle pistola cargada o vacia segun test

          "ladron pierde municion post disparar" in {
            usarPistola.ejecutar(EstadoResultado(ladron.copy(inventario = tienePistolaCargada), humanoGenerico)).estadoAtacante.inventario shouldBe
              Set[Item](pistolaCargada.copy(tipoArma = DeFuego(9)))
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
        val movComerOponente = Set[Movimiento](comerseAlOponente)

        "Cell se come a A17" in {
          val formaDeComerDeCell: tipos.FormaDeComer = (oponente, movs) => movs | oponente.listarMovimientos
          val cell: Guerrero = Guerrero("Cell", 110, 110, Monstruo(_.especie == Androide, formaDeComerDeCell), movComerOponente)
          val A17: Guerrero = Guerrero("Androide 17", 100, 100, Androide, Set[Movimiento](dejarseFajar))

          val postComida: EstadoResultado = comerseAlOponente.ejecutar(EstadoResultado(cell, A17))

          postComida.estadoAtacante.listarMovimientos shouldBe
            Set[Movimiento](comerseAlOponente, dejarseFajar)
          postComida.estadoOponente.estado shouldBe Muerto
        }

        "Cell se come a A17 y despues a A18" in {
          val formaDeComerDeCell: tipos.FormaDeComer = (oponente, movs) => movs | oponente.listarMovimientos
          val cell: Guerrero = Guerrero("Cell", 110, 110, Monstruo(_.especie == Androide, formaDeComerDeCell), movComerOponente)
          val A17: Guerrero = Guerrero("Androide 17", 100, 100, Androide, Set[Movimiento](dejarseFajar))
          val A18: Guerrero = Guerrero("Androide 18", 100, 100, Androide, Set[Movimiento](usarSemillaHermitanio))

          val cellPostPrimeraComida: Guerrero = comerseAlOponente.ejecutar(EstadoResultado(cell, A17)).estadoAtacante
          val postSegundaComida: EstadoResultado = comerseAlOponente.ejecutar(EstadoResultado(cellPostPrimeraComida, A18))

          postSegundaComida.estadoAtacante.listarMovimientos shouldBe
            Set[Movimiento](comerseAlOponente, dejarseFajar, usarSemillaHermitanio)
        }

        "Cell trata de comer a Goku" in {
          val formaDeComerDeCell: tipos.FormaDeComer = (oponente, movs) => movs | oponente.listarMovimientos
          val cell: Guerrero = Guerrero("Cell", 110, 110, Monstruo(_.especie == Androide, formaDeComerDeCell), movComerOponente)
          val goku: Guerrero = Guerrero("Goku", 100, 100, Saiyajin(), Set[Movimiento](usarSemillaHermitanio))

          val postComida:EstadoResultado = comerseAlOponente.ejecutar(EstadoResultado(cell, goku))

          postComida.estadoAtacante.listarMovimientos shouldBe cell.listarMovimientos
          postComida.estadoOponente.estado shouldBe Normal
        }

        "Goku trata de comer a Cell" in {
          val formaDeComerDeCell: tipos.FormaDeComer = (oponente, movs) => movs | oponente.listarMovimientos
          val cell: Guerrero = Guerrero("Cell", 110, 110, Monstruo(_.especie == Androide, formaDeComerDeCell), movComerOponente | Set[Movimiento](usarSemillaHermitanio))
          val goku: Guerrero = Guerrero("Goku", 100, 100, Saiyajin(), movComerOponente)

          val postComida:EstadoResultado = comerseAlOponente.ejecutar(EstadoResultado(goku, cell))

          postComida.estadoAtacante.listarMovimientos shouldBe goku.listarMovimientos
          postComida.estadoOponente.estado shouldBe Normal
        }

        "Cell trata de comer a un adroide más fuerte" in {
          val formaDeComerDeCell: tipos.FormaDeComer = (oponente, movs) => movs | oponente.listarMovimientos
          val cell: Guerrero = Guerrero("Cell", 110, 110, Monstruo(_.especie == Androide, formaDeComerDeCell), movComerOponente)
          val superA17: Guerrero = Guerrero("Super Androide 17", 200, 200, Androide, Set[Movimiento](dejarseFajar))

          val postComida: EstadoResultado = comerseAlOponente.ejecutar(EstadoResultado(cell, superA17))

          postComida.estadoAtacante.listarMovimientos shouldBe cell.listarMovimientos
          postComida.estadoOponente.estado shouldBe Normal
        }

        "Majin Buu se come a piccolo y gohan" in {
          val majinBuu: Guerrero = Guerrero("Majin Buu", 110, 110, Monstruo(_=>true, (op, _) => op.listarMovimientos), movComerOponente)
          val piccolo: Guerrero = Guerrero("Piccolo", 100, 100, Namekusein, Set[Movimiento](cargarKi))
          val gohan: Guerrero = Guerrero("Gohan", 100, 100, Saiyajin())

          val postPrimeraComida: EstadoResultado = comerseAlOponente.ejecutar(EstadoResultado(majinBuu, piccolo))
          val postSegundaComida: EstadoResultado = comerseAlOponente.ejecutar(postPrimeraComida.copy(estadoOponente = gohan))

          postPrimeraComida.estadoAtacante.listarMovimientos shouldBe majinBuu.movimientos | piccolo.movimientos
          postPrimeraComida.estadoOponente.estado shouldBe Muerto
          postSegundaComida.estadoAtacante.listarMovimientos shouldBe majinBuu.movimientos | gohan.movimientos
          postSegundaComida.estadoOponente.estado shouldBe Muerto
        }
      }

      "transformarse en mono" - {
        val movTransformaseEnMono = Set[Movimiento](convertirseEnMono)
        val tieneFotoLuna = Set[Item](FotoDeLuna)

        "gohan se transforma en mono" in {
          val gohan: Guerrero = Guerrero("Gohan", 10, 100, Saiyajin(), movTransformaseEnMono, tieneFotoLuna)
          convertirseEnMono.ejecutar(EstadoResultado(gohan, humanoGenerico)).estadoAtacante shouldBe gohan.copy(energiaMaxima = 300, energia = 300, especie = Saiyajin(true, Mono))
        }

        "SS goku (con cola) se transforma en mono" in {
          val goku: Guerrero = Guerrero("Goku", 10, 100, Saiyajin(true, SuperSaiyajin(20)), movTransformaseEnMono, tieneFotoLuna)
          convertirseEnMono.ejecutar(EstadoResultado(goku, humanoGenerico)).estadoAtacante shouldBe goku.copy(energia = 60, energiaMaxima = 60, especie = Saiyajin(true, Mono))
        }

        "goku (sin cola) no se transforma" in {
          val goku: Guerrero = Guerrero("Goku", 10, 100, Saiyajin(false), movTransformaseEnMono, tieneFotoLuna)
          convertirseEnMono.ejecutar(EstadoResultado(goku, humanoGenerico)).estadoAtacante shouldBe goku
        }

        "gohan (sin foto) no se transforma" in {
          val gohan: Guerrero = Guerrero("Gohan", 10, 100, Saiyajin(), movTransformaseEnMono)
          convertirseEnMono.ejecutar(EstadoResultado(gohan, humanoGenerico)).estadoAtacante shouldBe gohan
        }
      }
      "transformarseEnSS" - {
        val movTransEnSS = Set[Movimiento](transformarseEnSS)

        "goku se transforma en SS 1" in {
          val goku: Guerrero = Guerrero("Goku", 100, 100, Saiyajin(), movTransEnSS)

          transformarseEnSS.ejecutar(EstadoResultado(goku, humanoGenerico)).estadoAtacante shouldBe goku.copy(especie = Saiyajin(estado = SuperSaiyajin(100)), energiaMaxima = 500)
        }

        "goku se transforma en SS 2" in {
          val goku: Guerrero = Guerrero("Goku", 100, 100, Saiyajin(estado = SuperSaiyajin(20)), movTransEnSS)

          transformarseEnSS.ejecutar(EstadoResultado(goku, humanoGenerico)).estadoAtacante shouldBe goku.copy(especie = Saiyajin(estado = SuperSaiyajin(20, 2)), energiaMaxima = 200)
        }

        "goku no se puede transformar por falta de ki" in {
          val goku: Guerrero = Guerrero("Goku", 10, 100, Saiyajin(), movTransEnSS)

          transformarseEnSS.ejecutar(EstadoResultado(goku, humanoGenerico)).estadoAtacante shouldBe goku
        }

        "goku no se puede transformar por ser mono" in {
          val goku: Guerrero = Guerrero("Goku", 100, 100, Saiyajin(estado = Mono), movTransEnSS)

          transformarseEnSS.ejecutar(EstadoResultado(goku, humanoGenerico)).estadoAtacante shouldBe goku
        }

        "krillin no puede transformarse en SS" in {
          val krillin: Guerrero = Guerrero("Krillin", 100, 100, Humano, movTransEnSS)

          transformarseEnSS.ejecutar(EstadoResultado(krillin, humanoGenerico)).estadoAtacante shouldBe krillin
        }

        "SS goku queda inconsciente" in {
          val goku: Guerrero = Guerrero("Goku", 100, 100, Saiyajin(estado = SuperSaiyajin(20)), movTransEnSS)

          goku.quedarInconsciente shouldBe goku.copy(especie = Saiyajin(true, SaiyajinNormal), energiaMaxima = 20, energia = 20, estado = Inconsciente)
        }
      }
      "Fusion" - {
        val movUsarSemilla = Set[Movimiento](usarSemillaHermitanio)
        val tenerSemilla = Set[Item](SemillaDelHermitanio)
        val yajirobe: Guerrero = Guerrero("Yajirobe", 1, 100, Humano, movUsarSemilla, tenerSemilla)
        val movTransEnSS = Set[Movimiento](transformarseEnSS)
        val goku: Guerrero = Guerrero("Goku", 100, 100, Saiyajin(estado = SuperSaiyajin(20)), movTransEnSS)
        val A17: Guerrero = Guerrero("Androide 17", 100, 100, Androide, Set[Movimiento](dejarseFajar))

        "goku y yajirobe se fusionan" in {
          val gokuConFusion = goku.copy(movimientos = goku.movimientos + Fusion(yajirobe))
          Fusion(yajirobe).ejecutar(EstadoResultado(gokuConFusion, humanoGenerico)).estadoAtacante shouldBe
            Guerrero("Fusion de Goku y Yajirobe", 101, 200, Fusionado(gokuConFusion, yajirobe), movUsarSemilla | gokuConFusion.movimientos, yajirobe.inventario | goku.inventario)
        }

        "goku y A17 no se fusionan" in {
          val gokuConFusion = goku.copy(movimientos = goku.movimientos + Fusion(A17))
          Fusion(A17).ejecutar(EstadoResultado(gokuConFusion, humanoGenerico)).estadoAtacante shouldBe gokuConFusion
        }

        "Fusion de goku y yajirobe se deshace al quedar inconsciente" in {
          val gokuConFusion = goku.copy(movimientos = goku.movimientos + Fusion(yajirobe))
          val fusion = Fusion(yajirobe).ejecutar(EstadoResultado(gokuConFusion, humanoGenerico)).estadoAtacante

          fusion.quedarInconsciente shouldBe gokuConFusion.copy(estado = Inconsciente)
        }


        "Fusion de goku y yajirobe se deshace al quedar morir" in {
          val gokuConFusion = goku.copy(movimientos = goku.movimientos + Fusion(yajirobe))
          val fusion = Fusion(yajirobe).ejecutar(EstadoResultado(gokuConFusion, humanoGenerico)).estadoAtacante

          fusion.morir shouldBe gokuConFusion.copy(estado = Muerto, energia = 0)
        }
      }
      "Magia" - {
        val drenarEnergia: Movimiento = Magia("Drenar energia", {case EstadoResultado(a, o) => EstadoResultado(a, o.reducirKi(o.energia - 1))})
        val krillin: Guerrero = Guerrero("Krillin", 10, 10, Humano, Set[Movimiento](drenarEnergia))
        val sieteEsferas: Item = EsferasDelDragon(7)
        "Un krillin (con siete esferas) le quita toda la energia al oponente" in {
          drenarEnergia.ejecutar(EstadoResultado(krillin.copy(inventario = Set[Item](sieteEsferas)), humanoGenerico)) shouldBe
            EstadoResultado(krillin, humanoGenerico.copy(energia = 1))
        }

        "Un krillin falla en quitarle toda la energia al oponente" in {
          drenarEnergia.ejecutar(EstadoResultado(krillin, humanoGenerico)) shouldBe
            EstadoResultado(krillin, humanoGenerico)
        }
      }

      "Ataques" - {
        "Muchos Golpes Ninja" - {
          "goku muchos golpes ninja a krillin (menos ki)" in {
            val goku: Guerrero = Guerrero("Goku", 100, 100, Saiyajin(), Set[Movimiento](MGN))
            val krillin: Guerrero = Guerrero("Krillin", 80, 100, Humano)

            MGN.ejecutar(EstadoResultado(goku, krillin)) shouldBe EstadoResultado(goku, krillin.copy(energia = 60))
          }
          "goku muchos golpes ninja a vegeta (mas ki)" in {
            val goku: Guerrero = Guerrero("Goku", 100, 100, Saiyajin(), Set[Movimiento](MGN))
            val vegeta: Guerrero = Guerrero("vegeta", 120, 120, Saiyajin())

            MGN.ejecutar(EstadoResultado(goku, vegeta)) shouldBe EstadoResultado(goku.copy(energia = 80), vegeta)
          }
          "goku muchos golpes ninja a humano debil" in {
            val goku: Guerrero = Guerrero("Goku", 100, 100, Saiyajin(), Set[Movimiento](MGN))
            val humano: Guerrero = Guerrero("Humano", 10, 100, Humano)

            MGN.ejecutar(EstadoResultado(goku, humano)) shouldBe EstadoResultado(goku, humano.copy(energia = 0, estado = Muerto))
          }
          "Krillin ataca a androide 17" in {
            val krillin: Guerrero = Guerrero("Krillin", 100, 100, Humano, Set[Movimiento](MGN))
            val A17: Guerrero = Guerrero("Androide 17", 100, 100, Androide)

            MGN.ejecutar(EstadoResultado(krillin, A17)) shouldBe EstadoResultado(krillin.copy(energia = 90), A17)
          }
        }

        "Explotar" - {
          "cell explota" in {
            val cell: Guerrero = Guerrero("Cell", 10, 10, Monstruo({g => false}, {_.movimientos | _}), Set[Movimiento](explotar))
            val medidor: Guerrero = Guerrero("", 25, 25, Humano)

            explotar.ejecutar(EstadoResultado(cell, medidor)) shouldBe EstadoResultado(cell.copy(energia = 0, estado = Muerto), medidor.copy(energia = 5))
          }
          "A16 explota" in {
            val A16: Guerrero = Guerrero("Androide 16", 10, 10, Androide, Set[Movimiento](explotar))
            val medidor: Guerrero = Guerrero("", 35, 35, Humano)

            explotar.ejecutar(EstadoResultado(A16, medidor)) shouldBe EstadoResultado(A16.copy(energia = 0, estado = Muerto), medidor.copy(energia = 5))
          }

          "humano explota" in {
            val humano: Guerrero = Guerrero("humano", 10, 10, Humano, Set[Movimiento](explotar))
            val medidor: Guerrero = Guerrero("", 35, 35, Humano)

            explotar.ejecutar(EstadoResultado(humano, medidor)) shouldBe EstadoResultado(humano, medidor)
          }
          "A16 explota contra Piccolo" in {
            val A16: Guerrero = Guerrero("Androide 16", 10, 10, Androide, Set[Movimiento](explotar))
            val piccolo: Guerrero = Guerrero("Piccolo", 25, 35, Namekusein)

            explotar.ejecutar(EstadoResultado(A16, piccolo)) shouldBe EstadoResultado(A16.copy(energia = 0, estado = Muerto), piccolo.copy(energia = 1))
          }
        }
        "Onda de energia" - {
          val movKame = Set[Movimiento](kamehameha)
          val krillin: Guerrero = Guerrero("Krillin", 50, 50, Humano, movKame)
          "Krillin kamehameha a Yamcha" in {
            val yamcha: Guerrero = Guerrero("Yamcha", 50, 50, Humano)

            kamehameha.ejecutar(EstadoResultado(krillin, yamcha)) shouldBe EstadoResultado(krillin.copy(energia = 30), yamcha.copy(energia = 10))
          }
          "Krillin kamehameha a cell" in {
            val cell: Guerrero = Guerrero("Cell", 15, 15, Monstruo({g => false}, {_.movimientos | _}))

            kamehameha.ejecutar(EstadoResultado(krillin, cell)) shouldBe EstadoResultado(krillin.copy(energia = 30), cell.copy(energia = 5))
          }
          "Krillin debil falla kamehameha" in {
            kamehameha.ejecutar(EstadoResultado(krillin.copy(energia = 1), humanoGenerico)) shouldBe EstadoResultado(krillin.copy(energia = 1), humanoGenerico)
          }

          "Krillin kamehameha a Androide 17" in {
            val A17: Guerrero = Guerrero("Androide 17", 10, 60, Androide)

            kamehameha.ejecutar(EstadoResultado(krillin, A17)) shouldBe EstadoResultado(krillin.copy(energia = 30), A17.copy(energia = 50))
          }
        }
        "Genkidama" - {
          val movGenkidama = Set[Movimiento](Genkidama, dejarseFajar, kamehameha)
          val goku: Guerrero = Guerrero("Goku", 10, 10, Saiyajin(), movGenkidama)
          val medidor: Guerrero = Guerrero("", 10000, 10000, Humano)
          "Goku sin ser fajado no hace daño" in {
            Genkidama.ejecutar(EstadoResultado(goku, medidor)) shouldBe EstadoResultado(goku, medidor)
          }
          "Goku con un turno de ser fajado" in {
            Genkidama.ejecutar(EstadoResultado(goku.copy(turnosSiendoFajado = 1), medidor)) shouldBe EstadoResultado(goku.copy(turnosSiendoFajado = 0), medidor.copy(energia = 9990))
          }
          "Goku con dos turnos de ser fajado" in {
            Genkidama.ejecutar(EstadoResultado(goku.copy(turnosSiendoFajado = 2), medidor)) shouldBe EstadoResultado(goku.copy(turnosSiendoFajado = 0), medidor.copy(energia = 9900))
          }
          "Goku fajado un turno seguido de kamehameha fallido" in {
            val postKame: EstadoResultado = kamehameha.ejecutar(dejarseFajar.ejecutar(EstadoResultado(goku, medidor)))
            Genkidama.ejecutar(postKame) shouldBe EstadoResultado(goku, medidor)
          }
        }
      }
    }
    "Movimiento más efectivo" - {
      "Yajirobe quiere quedar con la mayor cantidad de energia" in {
        val yajirobe: Guerrero = Guerrero("Yajirobe", 10, 50, Humano,
          Set[Movimiento](usarSemillaHermitanio, usarEspada, dejarseFajar),
          Set[Item](SemillaDelHermitanio, espada))
        val criterio: EstadoResultado => tipos.RetornoCriterio = {case EstadoResultado(estadoAtacante, _) => estadoAtacante.energia}

        yajirobe.movimientoMasEfectivoContra(humanoGenerico)(criterio) shouldBe Some(usarSemillaHermitanio)
      }

      "Un tipo se quiere suicidar" in {
        val unTipo: Guerrero = Guerrero("Suicida", 10, 50, Humano,
          Set[Movimiento](usarSemillaHermitanio, dejarseFajar),
          Set[Item](SemillaDelHermitanio))
        //Criterio romperia si energia igual 0
        val criterio: EstadoResultado => tipos.RetornoCriterio = {case EstadoResultado(estadoAtacante, _) => 1.0 / estadoAtacante.energia}

        unTipo.movimientoMasEfectivoContra(humanoGenerico)(criterio) shouldBe Some(dejarseFajar)
      }

      "Trunks quiere hacer mierda a su oponente" in {
        val trunks: Guerrero = Guerrero("Trunks", 10, 50, Humano,
          Set[Movimiento](usarSemillaHermitanio, usarEspada, dejarseFajar),
          Set[Item](SemillaDelHermitanio, espada))
        //Criterio romperia si energia del oponente igual 0
        val criterio: EstadoResultado => tipos.RetornoCriterio = {case EstadoResultado(_, estadoOponente) => 1.0 / estadoOponente.energia}

        trunks.movimientoMasEfectivoContra(humanoGenerico)(criterio) shouldBe Some(usarEspada)
      }
    }
    "Pelear Round" - {
      val trunks: Guerrero = Guerrero("Trunks", 10, 500, Humano,
        Set[Movimiento](usarSemillaHermitanio, usarEspada, dejarseFajar),
        Set[Item](SemillaDelHermitanio, espada))
      val yajirobe: Guerrero = Guerrero("Yajirobe", 1, 2, Humano,
        Set[Movimiento](usarSemillaHermitanio, usarEspada, dejarseFajar),
        Set[Item](SemillaDelHermitanio, espada))
      "Trunks no hace nada y yajirobe lo mata" in {
        trunks.pelearRound(dejarseFajar)(yajirobe) shouldBe EstadoResultado(trunks.copy(turnosSiendoFajado = 1, energia = 0, estado = Muerto), yajirobe)
      }

      "Trunks se restaura y yajirobe hace lo mismo" in {
        trunks.pelearRound(usarSemillaHermitanio)(yajirobe) shouldBe EstadoResultado(trunks.copy(energia = 500), yajirobe.copy(energia = 2))
      }

      "Trunks ataca y yajirobe muere" in {
        trunks.pelearRound(usarEspada)(yajirobe) shouldBe EstadoResultado(trunks, yajirobe.copy(energia = 0, estado = Muerto))
      }
    }
    "plan de ataque contra" - {
      "Trunks quiere pelear 2 turnos haciendo tanto daño como pueda" in {
        val trunks: Guerrero = Guerrero("Trunks", 500, 500, Humano,
          Set[Movimiento](usarSemillaHermitanio, kamehameha, dejarseFajar),
          Set[Item](SemillaDelHermitanio))
        val goku: Guerrero = Guerrero("Goku", 500, 500, Saiyajin(false),
          Set[Movimiento](kamehameha, cargarKi))
        val criterio: Criterio = {case EstadoResultado(_, estadoOponente) => 1 / estadoOponente.energia.toDouble.max(0.00001)}

        /*
        Secuencia:
          Inicial -> (500, 500)
          t:kamehameha -> (480, 460) -> g:cargarKi -> (480, 500)
          t:kamehameha -> (460, 460) -> g:cargarKi -> (460, 500)
         */

        trunks.planDeAtaqueContra(goku, 2)(criterio) shouldBe Some(List[Movimiento](kamehameha, kamehameha))
      }
      "Trunks quiere pelear 2 turnos haciendo tanto daño como pueda teniendo su espada (goku con cola)" in {
        val trunks: Guerrero = Guerrero("Trunks", 500, 500, Humano,
          Set[Movimiento](usarSemillaHermitanio, usarEspada, dejarseFajar),
          Set[Item](SemillaDelHermitanio, espada))
        val goku: Guerrero = Guerrero("Goku", 500, 500, Saiyajin(),
          Set[Movimiento](kamehameha, cargarKi))
        val criterio: Criterio = {case EstadoResultado(_, estadoOponente) => 1 / estadoOponente.energia.toDouble.max(0.00001)}

        /*
        Secuencia:
          Inicial -> (500, 500)
          usarEspada -> (500, 1) -> cargarKi -> (500, 101)
          usarEspada -> (500, 0) -> (Muerto) -> (500, 0)
         */
        trunks.planDeAtaqueContra(goku, 2)(criterio) shouldBe Some(List[Movimiento](usarEspada, usarEspada))
      }
      "Trunks quiere pelear 4 turnos haciendo tanto daño como pueda teniendo su espada (goku con cola)" in {
        val trunks: Guerrero = Guerrero("Trunks", 500, 500, Humano,
          Set[Movimiento](usarSemillaHermitanio, usarEspada, dejarseFajar),
          Set[Item](SemillaDelHermitanio, espada))
        val goku: Guerrero = Guerrero("Goku", 500, 500, Saiyajin(),
          Set[Movimiento](kamehameha, cargarKi))
        val criterio: Criterio = {case EstadoResultado(_, estadoOponente) => 1 / estadoOponente.energia.toDouble.max(0.00001)}

        /*
        Secuencia:
          Inicial -> (500, 500)
          usarEspada -> (500, 1) -> cargarKi -> (500, 101)
          usarEspada -> (500, 0) -> (Muerto) -> (500, 0)
         */
        trunks.planDeAtaqueContra(goku, 4)(criterio) shouldBe None
      }
    }
    "Pelear contra" - {
      val goku: Guerrero = Guerrero("Goku", 500, 500, Saiyajin(),
        Set[Movimiento](kamehameha, cargarKi))
      "Trunks vs Goku (No termina)" in {
        val trunks: Guerrero = Guerrero("Trunks", 500, 500, Humano,
          Set[Movimiento](usarSemillaHermitanio, kamehameha, dejarseFajar),
          Set[Item](SemillaDelHermitanio))
        val planDeAtaque: List[Movimiento] = List(kamehameha, kamehameha)

        trunks.pelearContra(goku)(planDeAtaque) shouldBe Peleando(trunks.copy(energia = 460), goku)
      }
      "Trunks vs Goku (Gana Trunks)" in {
        val trunks: Guerrero = Guerrero("Trunks", 500, 500, Humano,
          Set[Movimiento](usarSemillaHermitanio, usarEspada, dejarseFajar),
          Set[Item](SemillaDelHermitanio, espada))
        val planDeAtaque: List[Movimiento] = List(usarEspada, usarEspada)

        trunks.pelearContra(goku)(planDeAtaque) shouldBe Terminada(trunks)
      }

      "Trunks vs Goku (Gana Trunks y no sigue)" in {
        val trunks: Guerrero = Guerrero("Trunks", 400, 500, Humano,
          Set[Movimiento](usarSemillaHermitanio, usarEspada, dejarseFajar),
          Set[Item](SemillaDelHermitanio, espada))
        val planDeAtaque: List[Movimiento] = List(usarEspada, usarEspada, usarSemillaHermitanio)

        trunks.pelearContra(goku)(planDeAtaque) shouldBe Terminada(trunks)
      }
      "Trunks vs Goku (Gana Goku y no sigue)" in {
        val trunks: Guerrero = Guerrero("Trunks", 1, 500, Humano,
          Set[Movimiento](usarSemillaHermitanio, usarEspada, dejarseFajar),
          Set[Item](SemillaDelHermitanio, espada))
        val planDeAtaque: List[Movimiento] = List(dejarseFajar, usarSemillaHermitanio)

        trunks.pelearContra(goku)(planDeAtaque) shouldBe Terminada(goku.copy(energia = 480))
      }
      "Cell vs Goku (Kaboom)" in {
        val cell: Guerrero = Guerrero("Cell", 500, 500, Monstruo({g => false}, {_.movimientos | _}), Set[Movimiento](explotar))
        val planDeAtaque: List[Movimiento] = List(explotar)

        cell.pelearContra(cell)(planDeAtaque) shouldBe Terminada(cell.copy(energia = 0, estado = Muerto))
      }
    }
  }
}
