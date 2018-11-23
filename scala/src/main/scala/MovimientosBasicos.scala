import Modelo._

object MovimientosBasicos {

  val dejarseFajar: Movimiento = Movimiento("dejarseFajar", (g1: Guerrero, g2: Guerrero) => (g1.copy(), g2.copy()))

  val cargarKi: Movimiento = Movimiento("cargarKi", (g1: Guerrero, g2: Guerrero) =>
    if (puedeUsarMov(g1)) {
    g1.tipo match {
      case Androide(_) => (g1.copy(), g2.copy())
      case Sayajin(ssjLvl, _, _) if ssjLvl > 0 => (g1.copy(ki = g1.ki + 150 * ssjLvl), g2.copy())
      case _ => (actualizarEstado(g1.copy(ki = g1.ki + 100)), g2.copy())
    }}else {
        (g1.copy(), g2.copy())
      }
  )

  def usarItem(unItem: Item): Movimiento = Movimiento("usarItem", (g1: Guerrero, g2: Guerrero) =>
    if (g1.tieneElItem(unItem) && puedeUsarMov(g1)) {
      (unItem,g2.tipo) match {
        // TODO: no vale ese "equals", Androide es el companion object de la clase (es un objeto y siempre es distinto a un tipo de androide)
        //   Para hacer eso mismo es más cómodo usar pattern matching con algo como "Androide(xxx)"
        //   Para poder hacer pattern matching del guerrero junto con el item tenés que ponerlos en una tupla o algún otro objeto donde ambos estén juntos
        // DONE (Santi)
        case (Arma(Roma),Androide(_)) => (g1.copy(), g2.copy())
        case (Arma(Roma),_) => (g1.copy(), g2.copy(estado = Inconsciente))
        case (Arma(Filosa),Sayajin(lvl, true, true)) =>(g1.copy(), g2.copy(ki = 1, tipo = Sayajin(lvl, false, false), estado = Inconsciente))
        case (Arma(Filosa),Sayajin(lvl, true, false)) => (g1.copy(), g2.copy(ki = 1, tipo = Sayajin(lvl, false, false)))
        case (Arma(Filosa),_) => (g1.copy(), g2.copy(ki = (g2.ki - (g1.ki / 100)).max(0)))
        case (Arma(Fuego(muni)),_) if muni > 0 =>
          g2 match {
            case Guerrero(_, _, _, _, Humano, _, _) => (g1.copy(), actualizarEstado(g2.copy(ki = (g2.ki - 20).max(0)))) // Actualizar muni en g1
            case Guerrero(_, _, _, _, Namekusein, _, Inconsciente) => (g1.copy(), actualizarEstado(g2.copy(ki = (g2.ki - 10).max(0)))) //Igual q arriba
            case _ => (g1.copy(), g2.copy())
          }
        case (SemillaHermitanio,_) => (g1.copy(ki = g1.kiMax), g2.copy())
        case _ => (g1.copy(), g2.copy())
      }
    } else {
      (g1.copy(), g2.copy())
    })

  val comerOponente: Movimiento = Movimiento("comerOponente", (g1:Guerrero, g2:Guerrero) =>
    if (puedeUsarMov(g1)) {
    g1.tipo match{
      case Monstruo(_,formaDeDigerir) => (formaDeDigerir(g1,g2.movs), actualizarEstado(g2.copy(estado = Muerto, ki = 0)))
      // TODO: no es necesario el copy vacío, esa es la gran ventaja de que sean inmutables ;)
      case _ => (g1.copy(), g2.copy())
    }}else (g1.copy(), g2.copy())
  )

  val sumarMovimientos: FormaDeDigerir = (g1: Guerrero, movs: List[Movimiento]) =>
    g1.tipo match{
      case Monstruo(movsAdq,forma) => g1.copy(tipo = Monstruo(movsAdq ++ movs, forma))
      case _ => g1.copy()
    }


  val pisarMovimientos: FormaDeDigerir = (g1: Guerrero, movs: List[Movimiento]) =>
    g1.tipo match{
      case Monstruo(movsAdq,forma) => g1.copy(tipo = Monstruo(movs, forma))
      case _ => g1.copy()
    }



  val convertirseEnSSJ: Movimiento = Movimiento("convertirseSSJ", (g1:Guerrero, g2:Guerrero) =>
    if (puedeUsarMov(g1)) {
    g1.tipo match{

      case Sayajin(ssjLvl, tieneCola, false) if g1.ki >= (g1.kiMax*0.5) => (g1.copy(kiMax = g1.kiMax*5*(ssjLvl+1),
        tipo = Sayajin(ssjLvl+1,tieneCola, esMono = false)), g2.copy())

      case _ => (g1.copy(), g2.copy())
    }
    }else (g1.copy(), g2.copy())
  )

  def muchosGolpesNinja(Fisico: TipoAtaque): Movimiento = Movimiento("GolpesNinja", (g1, g2) => if (puedeUsarMov(g1)) { (g1.tipo, g2.tipo) match{
    case (Humano, Androide(_)) => (g1.copy(ki = (g1.ki - 10).max(0)), g2.copy()) // Se lastima los Deditos
    case (_,_) => if(g1.ki > g2.ki){(g1.copy(),g2.copy(ki=(g2.ki-20).max(0)))}else{(actualizarEstado(g1.copy(ki=(g1.ki-20).max(0))),g2.copy())}
  }
  }else (g1.copy(), g2.copy())
    )

  def onda(energia:Int): Movimiento = Movimiento("OndaEnergia", (g1:Guerrero, g2: Guerrero) =>
    if (puedeUsarMov(g1)) {(g1.tipo, g2.tipo) match{
      case (Androide(bata), Monstruo(_,_)) =>
        (actualizarEstado(g1.copy(tipo = Androide(bata-energia))),actualizarEstado(g2.copy(ki = (g2.ki-(energia*2)).max(0))))
      case (_,Monstruo(_,_)) =>  (actualizarEstado(g1.copy(ki = (g1.ki-energia).max(0))), actualizarEstado(g2.copy(ki = (g2.ki-(energia/2)).max(0))))

      case (Androide(batt),_) =>
        (actualizarEstado(g1.copy(tipo = Androide((batt-energia).max(0)))),actualizarEstado(g2.copy(ki = (g2.ki-(energia*2)).max(0))))
      case (_,_) if g1.ki > energia =>
        (actualizarEstado(g1.copy(ki = g1.ki-energia)),actualizarEstado(g2.copy(ki = (g2.ki-(energia*2)).max(0))))
      case (_,_) =>
        (g1.copy(), g2.copy())
    }}else (g1.copy(), g2.copy())
  )

  // Criterio establecido para el Requerimiento #2
  val mayorPuntosDeKi: Criterio = (g1: Guerrero, g2: Guerrero) => g2.ki

  val prioridadAtaque: Criterio = (g1: Guerrero, g2: Guerrero) => (g1.ki - g2.ki).abs

  val actualizarEstado: Guerrero => Guerrero = (g1:Guerrero) =>

    g1.tipo match{

      case Androide(battery) => if (battery <= 0) {g1.copy(estado = Muerto, tipo = Androide(0))} else {g1.copy()}
      case _ =>  if (g1.ki <= 0) {g1.copy(estado = Muerto, ki = 0)} else if (g1.ki > g1.kiMax){g1.copy(ki = g1.kiMax)} else {g1.copy()}

    }

  val puedeUsarMov: Guerrero => Boolean = (g1:Guerrero) =>
    if((g1.ki > 0) && (!g1.estaMuerto)){true } else{ false}

}