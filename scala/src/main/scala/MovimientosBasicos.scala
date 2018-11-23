import Modelo._

object MovimientosBasicos {

  val dejarseFajar: Movimiento = Movimiento("dejarseFajar", (g1: Guerrero, g2: Guerrero) => (g1, g2))

  val cargarKi: Movimiento = Movimiento("cargarKi", (g1: Guerrero, g2: Guerrero) =>
    if (puedeUsarMov(g1)) {
      // TODO: si está inconsciente no puede tampoco
      // (el tema de usar el "puedeUsarMov" en el movimiento hace que lo tengas que repetir en cada uno, cuidado)
      g1.tipo match {
        case Androide(_) => (g1, g2)
        case Sayajin(ssjLvl, _, _) if ssjLvl > 0 => (g1.copy(ki = g1.ki + 150 * ssjLvl), g2)
        case _ => (actualizarEstado(g1.copy(ki = g1.ki + 100)), g2)
      }
    } else {
      (g1, g2)
    }
  )

  def usarItem(unItem: Item): Movimiento = Movimiento("usarItem", (g1: Guerrero, g2: Guerrero) =>
    if (g1.tieneElItem(unItem) && puedeUsarMov(g1)) {
      (unItem, g2.tipo) match {
        case (Arma(Roma), Androide(_)) => (g1, g2)
        // TODO: agregada la condición sobre el ki < 300
        case (Arma(Roma), _) if g2.ki < 300 => (g1, g2.copy(estado = Inconsciente))
        case (Arma(Filosa), Sayajin(lvl, true, true)) => (g1, g2.copy(ki = 1, tipo = Sayajin(lvl, false, false), estado = Inconsciente))
        case (Arma(Filosa), Sayajin(lvl, true, false)) => (g1, g2.copy(ki = 1, tipo = Sayajin(lvl, false, false)))
        // TODO: el ".max(0)" es una de las validaciones repetidas, debería estar en el guerrero
        case (Arma(Filosa), _) => (g1, g2.copy(ki = (g2.ki - (g1.ki / 100)).max(0)))
        case (Arma(Fuego(muni)), _) if muni > 0 =>
          // TODO g1 debería tener la munición en el inventario
          g2 match {
            case Guerrero(_, ki, _, _, Humano, _, _) => (g1, actualizarEstado(g2.copy(ki = (ki - 20).max(0)))) // Actualizar muni en g1
            case Guerrero(_, _, _, _, Namekusein, _, Inconsciente) => (g1, actualizarEstado(g2.copy(ki = (g2.ki - 10).max(0)))) //Igual q arriba
            // TODO: te olvidaste de tu comentario :P (g1 pierde la municion luego de usarla)
            case _ => (g1, g2)
          }
        // TODO la semilla es la única que vale cuando está inconsciente
        case (SemillaHermitanio, _) => (g1.copy(ki = g1.kiMax), g2)
        case _ => (g1, g2)
      }
    } else {
      (g1, g2)
    })

  val comerOponente: Movimiento = Movimiento("comerOponente", (g1: Guerrero, g2: Guerrero) =>
    if (puedeUsarMov(g1)) {
      g1.tipo match {
        case Monstruo(_, formaDeDigerir) => (
          // TODO: deberían ser solo androides en este caso
          formaDeDigerir(g1, g2.movs),
          actualizarEstado(g2.copy(estado = Muerto, ki = 0))
        )
        case _ => (g1, g2)
      }
    } else (g1, g2)
  )

  val sumarMovimientos: FormaDeDigerir = (g1: Guerrero, movs: List[Movimiento]) =>
    g1.tipo match {
      case Monstruo(movsAdq, forma) => g1.copy(tipo = Monstruo(movsAdq ++ movs, forma))
      case _ => g1
    }

  val pisarMovimientos: FormaDeDigerir = (g1: Guerrero, movs: List[Movimiento]) =>
    g1.tipo match {
      case Monstruo(movsAdq, forma) => g1.copy(tipo = Monstruo(movs, forma))
      case _ => g1
    }

  val convertirseEnSSJ: Movimiento = Movimiento("convertirseSSJ", (g1: Guerrero, g2: Guerrero) =>
    if (puedeUsarMov(g1)) {
      g1.tipo match {
        case Sayajin(ssjLvl, tieneCola, false) if g1.ki >= (g1.kiMax * 0.5) => (
          g1.copy(
            kiMax = g1.kiMax * 5 * (ssjLvl + 1),
            tipo = Sayajin(ssjLvl + 1, tieneCola, esMono = false)
          ),
          g2
        )
        case _ => (g1, g2)
      }
    } else (g1, g2)
  )

  // TODO el parámetro de la función no se usa
  def muchosGolpesNinja(Fisico: TipoAtaque): Movimiento = Movimiento("GolpesNinja", (g1, g2) =>
    if (puedeUsarMov(g1)) {
      (g1.tipo, g2.tipo) match {
        case (Humano, Androide(_)) => (g1.copy(ki = (g1.ki - 10).max(0)), g2) // Se lastima los Deditos
        case (_, _) => if (g1.ki > g2.ki) {
          (g1, g2.copy(ki = (g2.ki - 20).max(0)))
        } else {
          (actualizarEstado(g1.copy(ki = (g1.ki - 20).max(0))), g2)
        }
      }
    } else (g1, g2)
  )

  def onda(energia: Int): Movimiento = Movimiento("OndaEnergia", (g1: Guerrero, g2: Guerrero) =>
    if (puedeUsarMov(g1)) {
      (g1.tipo, g2.tipo) match {
        case (Androide(bata), Monstruo(_, _)) =>
          (actualizarEstado(g1.copy(tipo = Androide(bata - energia))), actualizarEstado(g2.copy(ki = (g2.ki - (energia * 2)).max(0))))
        case (_, Monstruo(_, _)) => (actualizarEstado(g1.copy(ki = (g1.ki - energia).max(0))), actualizarEstado(g2.copy(ki = (g2.ki - (energia / 2)).max(0))))
        case (Androide(batt), _) =>
          (actualizarEstado(g1.copy(tipo = Androide((batt - energia).max(0)))), actualizarEstado(g2.copy(ki = (g2.ki - (energia * 2)).max(0))))
        case (_, _) if g1.ki > energia =>
          (actualizarEstado(g1.copy(ki = g1.ki - energia)), actualizarEstado(g2.copy(ki = (g2.ki - (energia * 2)).max(0))))
        case (_, _) =>
          (g1, g2)
      }
    } else (g1, g2)
  )

  // Criterio establecido para el Requerimiento #2
  val mayorPuntosDeKi: Criterio = (g1: Guerrero, g2: Guerrero) => g2.ki

  val prioridadAtaque: Criterio = (g1: Guerrero, g2: Guerrero) => (g1.ki - g2.ki).abs

  // TODO estas protecciones estarían mejor dentro del guerrero
  // - los cambios en el ki por ejemplo podrías hacerlo en un mensaje "updateKi" que haga las validaciones correspondientes
  val actualizarEstado: Guerrero => Guerrero = (g1: Guerrero) =>
    g1.tipo match {
      case Androide(battery) => if (battery <= 0) {
        g1.copy(estado = Muerto, tipo = Androide(0))
      } else {
        g1
      }
      case _ => if (g1.ki <= 0) {
        g1.copy(estado = Muerto, ki = 0)
      } else if (g1.ki > g1.kiMax) {
        g1.copy(ki = g1.kiMax)
      } else {
        g1
      }
    }

  // TODO: borré el if, no era necesario y cambie el val por un def
  def puedeUsarMov(g1: Guerrero): Boolean =
    (g1.ki > 0) && (!g1.estaMuerto)

}