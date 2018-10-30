import scala.util.Try

object tipos{
  type Accion = Peleando => Resultado
}

case class Guerrero(nombre: String,
                    energia : Int,
                    energiaMaxima : Int,
                    especie : Especie,
                    movimientos : Map[String, Movimiento] = Map[String, Movimiento](),
                    inventario : Map[String, Item] = Map[String, Item](),
                    turnosSiendoFajado : Short = 0,
                    estado : Estado = Normal) {
  
  require(energia > 0, "La energia no puede ser negativa")
  require(energiaMaxima > energia, "La energia no puede superar el maximo")
  require(energiaMaxima > 0, "La energia maxima no puede ser negativa")
  require(turnosSiendoFajado >= 0, "turnosSiendoFajado debe ser positivo o 0")
  require(energia == 0 && estado == Normal, "El guerrero no puede estar bien sin energia")

  def hacerAlgo(f: Guerrero => Guerrero): Guerrero = this.estado match {
    case Muerto => this
    case _ => f(this)
  }

  def cargarKi(): Guerrero = hacerAlgo(_.especie match {
    case Androide => this
    case Saiyajin(_, _, nivelSS) if nivelSS > 0 => this.copy(energia = (energia + 150 * nivelSS) max energiaMaxima)
    case _ => this.copy(energia = (energia + 100) max energiaMaxima)
  })

  def restaurar(): Guerrero = hacerAlgo(_.copy(energia = energiaMaxima))

  def reducirKi(cantidad: Int): Guerrero = hacerAlgo(g =>
    if(g.energia - cantidad <= 0) g.copy(energia = 0).copy(estado = Muerto)
    else g.copy(energia = g.energia - cantidad))

  def incrementarMaximo(cantidad: Int): Guerrero = hacerAlgo(g => g.copy(energiaMaxima = g.energiaMaxima + cantidad))

  def reducirMaximo(cantidad: Int): Guerrero =
    hacerAlgo(g => g.copy(energia = g.energia.min(g.energiaMaxima)).copy(energiaMaxima = (g.energiaMaxima - cantidad).max(1)))

  def cambiarEstado(nuevoEstado: Estado): Guerrero = hacerAlgo(g => g.copy(estado = nuevoEstado))

  def morir(): Guerrero = (especie match {
    case Fusionado(original, _) => original
    case _ => this
  }).copy(estado = Muerto)

  def quedarInconsiente(): Guerrero = (especie match {
    case Fusionado(original, _) => original
    case _ => this
  }).copy(estado = Inconsciente)

  def actualizarMunicion(nombre:String, nuevaCantidad: Int): Guerrero = {
    val arma: Option[Item] = inventario.get(nombre)
    if(!arma.isEmpty) arma.get match {
      case Arma(nombre, DeFuego(_)) => copy(inventario = inventario - nombre).copy(inventario = inventario + (nombre -> Arma(nombre, DeFuego(nuevaCantidad))))
      case _ => this
    }
    else this
  }

  def usarItem(item: String)(oponente:Guerrero): Resultado = {
    if(estado != Muerto) {
      val resultadoBase = Resultado(this, oponente)
      inventario.get(item).map({ i =>
        i match {
          case i @ SemillaDelHermitanio => i.usar(resultadoBase)
          case otro if this.estado == Inconsciente => resultadoBase
          case Arma(_, DeFuego(municion)) if municion == 0 => resultadoBase
          case arma@Arma(nombre, DeFuego(municion)) => arma.usar(Resultado(actualizarMunicion(nombre, municion - 1), oponente))
          case otro => otro.accion(resultadoBase)
        }
      }).getOrElse(resultadoBase)
    }
    else Resultado(this, oponente)
  }

  def tieneItem(item: String): Boolean = inventario.exists(_ == item)

  def movimientoMasEfectivoContra(oponente: Guerrero)(criterio: Resultado => Int): Option[Movimiento] = {
    val estadoInicial: Resultado = Resultado(this, oponente)
    val filtrarMovimientosValidos: ((String, Movimiento)) => Boolean =
      (movimiento) => criterio(movimiento._2.accion(estadoInicial)) > 0
    val usarCriterio: ((String, Movimiento)) => Int =
      (movimiento) => criterio(movimiento._2.accion(estadoInicial))

    Try(this.movimientos.filter(filtrarMovimientosValidos).maxBy[Int](usarCriterio)).toOption
  }

  def pelearRound(movimiento: Movimiento)(oponente: Guerrero): Resultado = {
    //Este criterio solo busca la mayor ventaja (Posible solucion: cambiar criterio para que devuelva punto flotante y hacer la division de la energia)
    val criterioDeMasEnergia: Resultado => Int = {case Resultado(atacante, oponente) => atacante.energia - oponente.energia}
    val primerAtaque: Resultado = movimiento.accion(Resultado(this, oponente))
    primerAtaque.estadoOponente.movimientoMasEfectivoContra(primerAtaque.estadoAtacante)(criterioDeMasEnergia).
      map(unMovimiento => unMovimiento.accion(primerAtaque)).getOrElse[Resultado](primerAtaque)
  }

  def planDeAtaqueContra(oponente: Guerrero, cantTurnos: Int)(criterio: Resultado => Int): Option[Seq[(NombreMovimiento, Movimiento)]] = {
    val movimientoMasEfectivo: Option[(NombreMovimiento, Movimiento)] = this.movimientoMasEfectivoContra(oponente)(criterio)
    if(movimientoMasEfectivo.isEmpty || cantTurnos == 0)
      None
    else {
      val estadoPostRound = pelearRound(movimientoMasEfectivo.get._2)(oponente)

      Option(movimientoMasEfectivo.toSeq).
        map(_ ++ estadoPostRound.estadoAtacante.
                    planDeAtaqueContra(estadoPostRound.estadoOponente, cantTurnos-1)(criterio).
                  getOrElse(Seq[(NombreMovimiento, Movimiento)]() ))
    }
  }

}


sealed trait Especie {
  def unapply(arg: Guerrero): Option[Guerrero] = if(arg.especie == this) Some(arg) else None
}
case object Humano extends Especie
case class Saiyajin(tieneCola : Boolean = true,
                    esMono : Boolean = false,
                    nivelSS : Int = 0) extends Especie{
  require(nivelSS >= 0, "El nivel de super saiyajin no puede ser negativo")
  require((!esMono || tieneCola), "Un saiyajin no puede ser mono sin tener cola")

  def cortarCola(guerrero: Guerrero): Guerrero = guerrero.especie match {
    case Saiyajin(true, true, _) => guerrero.reducirKi(guerrero.energia - 1).copy(especie = Saiyajin(false, false, 0))
    case Saiyajin(true, _, nivelSS) => guerrero.reducirKi(guerrero.energia - 1).copy(especie = Saiyajin(false, false, nivelSS))
    case _ => guerrero
  }

  def transformarEnMono(guerrero: Guerrero): Guerrero = guerrero.especie match {
    case Saiyajin(true, false, _) if guerrero.tieneItem(FotoDeLuna.nombre) =>
      this.dejarDeSerSuperSaiyajin(guerrero).incrementarMaximo(guerrero.energiaMaxima * 2).restaurar()
    case _ => guerrero
  }

  def convertiseEnSuperSaiyajin(guerrero: Guerrero): Guerrero =
    if(guerrero.energia >= guerrero.energiaMaxima / 2) guerrero.copy(especie = this.copy(nivelSS = nivelSS + 1)).incrementarMaximo(5 * nivelSS)
    else guerrero

  def dejarDeSerSuperSaiyajin(guerrero: Guerrero): Guerrero = guerrero.copy(especie = this.copy(nivelSS = 0)).reducirMaximo(nivelSS * 5)
}
case object Androide extends Especie
case object Namekusein extends Especie
case class Monstruo(formaDeComer : (Guerrero, Map[String, Movimiento]) => Map[String, Movimiento],
                    movimientosDevorados : Map[String, Movimiento] = Map[String, Movimiento]()) extends Especie {

  def devorar(self: Guerrero, oponente: Guerrero): Resultado =
    Resultado(self.copy(especie = this.copy(movimientosDevorados = formaDeComer(oponente, movimientosDevorados))),
              oponente.morir())
}
case class Fusionado(original : Guerrero, amigo : Guerrero) extends Especie {

  def obtenerFusion(): Guerrero = Guerrero("Fusion de " + original.nombre + " y " + amigo.nombre,
    original.energia + amigo.energia,
    original.energiaMaxima + amigo.energiaMaxima,
    this,
    original.movimientos ++ amigo.movimientos,
    original.inventario ++ amigo.inventario
    )
}


trait Item {
  val nombre: String
  val accion: tipos.Accion

  def usar(resultado: Resultado): Resultado = {
    case Terminada => resultado
    case _ => accion(resultado)
  }
}
case class ItemBasico(nombre: String, accion: tipos.Accion = {_}) extends Item
case object SemillaDelHermitanio extends Item {
  val nombre: String = "Semilla del hermitaño"
  val accion: tipos.Accion = res => res.copy(estadoAtacante = res.estadoAtacante.restaurar)
}
case class Arma(nombre: String, tipoArma: TipoArma) extends Item {
  val accion: tipos.Accion = tipoArma.procesar(_)
}
case object FotoDeLuna extends Item {
  val nombre: String = "Foto de la luna"
  val accion: tipos.Accion = _
}

trait Movimiento {
  val accion: tipos.Accion

  def ejecutar(resultado: Resultado): Resultado = {
    case Terminada => resultado
    case _ => accion(resultado)
  }
}
case class UsarItem(nombre: String, item: Item) extends Movimiento {
  val accion: tipos.Accion = res => if(res.estadoAtacante.tieneItem(item.nombre)) item.accion(Resultado) else res
}
case class MovimientoSimple(nombre: String,
                      accion: tipos.Accion) extends Movimiento

case class Fusion(amigo: Guerrero) extends Movimiento {
  require(amigo.especie == Humano || amigo.especie.getClass == Saiyajin || amigo.especie == Namekusein, "Solo los humanos, saiyajins y namekuisein pueden fusionarse")
  val accion: tipos.Accion = res => Resultado(Fusionado(res.estadoAtacante, amigo).obtenerFusion, res.estadoOponente)
}

case class Ataque(accion: tipos.Accion, tipoAtaque: TipoAtaque)


sealed trait Estado {
  def unapply(arg: Guerrero): Option[Guerrero] = if(arg.estado == this) Some(arg) else None
}
case object Normal extends Estado
case object Inconsciente extends Estado
case object Muerto extends Estado


sealed trait Resultado
object Resultado {
  def apply(atacante: Guerrero, oponente: Guerrero): Resultado =
    if(oponente.estado == Muerto) Terminada(atacante)
    else if(atacante.estado == Muerto) Terminada(oponente)
    else Peleando(atacante, oponente)
}
case class Peleando(estadoAtacante: Guerrero, estadoOponente: Guerrero) extends Resultado
case class Terminada(ganador: Guerrero) extends Resultado


sealed trait TipoAtaque
case object Energia extends TipoAtaque
case object Fisico extends TipoAtaque


sealed trait TipoArma {
  def procesar(res: Peleando): Resultado
}
case object Filosa extends TipoArma {
  override def procesar(res: Peleando): Resultado = {
    res.estadoOponente.especie match {
      case saiyajin @ Saiyajin(true, _, _) => Resultado(res.estadoAtacante, saiyajin.cortarCola(res.estadoOponente))
      case _ => Resultado(res.estadoAtacante, res.estadoOponente.reducirKi(res.estadoAtacante.energia / 100))
    }
  }
}
case class DeFuego(municion: Int) extends TipoArma {
  override def procesar(res: Peleando): Resultado = {
    val estadoAtacante: Guerrero = res.estadoAtacante
    res.estadoOponente match {
      case Humano(oponente) => Resultado(estadoAtacante, oponente.reducirKi(20))
      case Namekusein(oponente) if oponente.estado == Inconsciente => Resultado(estadoAtacante, oponente.reducirKi(10))
      case _ => res
    }

  }
}
case object Roma extends TipoArma {
  override def procesar(res: Peleando): Resultado = {
    if(res.estadoOponente.especie != Androide && res.estadoOponente.energia < 300) Resultado(res.estadoAtacante, res.estadoOponente.cambiarEstado(Inconsciente))
    else res
  }
}