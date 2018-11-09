import tipos.{Accion, RetornoCriterio}

object tipos{
  type Accion = EstadoResultado => EstadoResultado
  type RetornoCriterio = Double
}

object EspecieConMagia {
  def unapply(arg: Guerrero): Option[Guerrero] = if (arg.especie == Namekusein || arg.especie.isInstanceOf[Monstruo]) Some(arg) else None

  val tieneSieteEsferas:  Guerrero => Boolean = arg => arg.tieneItem("Esferas del dragon (7)")
}

object PuedeFusionarse {
  def unapply(arg: Guerrero): Option[Guerrero] = if(arg.especie == Humano || arg.especie.isInstanceOf[Saiyajin] || arg.especie == Namekusein) Some(arg) else None
}

case class Guerrero(nombre: String,
                    energia : Int,
                    energiaMaxima : Int,
                    especie : Especie,
                    movimientos : Map[String, Movimiento] = Map[String, Movimiento](),
                    inventario : Map[String, Item] = Map[String, Item](),
                    estado : Estado = Normal,
                    turnosSiendoFajado : Int = 0) {
  
  require(energia >= 0, "La energia no puede ser negativa")
  require(energiaMaxima >= energia, "La energia no puede superar el maximo")
  require(energiaMaxima > 0, "La energia maxima no puede ser negativa ni cero")
  require(turnosSiendoFajado >= 0, "turnosSiendoFajado debe ser positivo o 0")
  require((estado == Muerto && energia == 0) || (estado != Muerto && energia > 0), "El guerrero no puede estar bien sin energia")

  def hacerAlgo(f: Guerrero => Guerrero): Guerrero = this.estado match {
    case Normal => f(this).copy(turnosSiendoFajado = 0)
    case _ => this
  }

  def dejarseFajar: Guerrero = estado match {
    case Normal => this.copy(turnosSiendoFajado = turnosSiendoFajado + 1)
    case _ => this
  }

  def cargarKi: Guerrero = hacerAlgo(_.especie match {
    case Androide => this
    case Saiyajin(_, SuperSaiyajin(_, nivel)) => this.copy(energia = (energia + 150 * nivel) min energiaMaxima)
    case _ => this.copy(energia = (energia + 100) min energiaMaxima)
  })

  val restauracion: Guerrero => Guerrero = _.copy(energia = energiaMaxima, estado = Normal)

  def restaurar: Guerrero = hacerAlgo(restauracion)

  def restaurarPorSemilla: Guerrero = estado match {
    case Muerto => this
    case _ => restauracion(this)
  }

  def reducirKi(cantidad: Int): Guerrero =
    if(energia - cantidad <= 0) copy(energia = 0, estado = Muerto)
    else copy(energia = energia - cantidad)

  def incrementarKi(cantidad: Int): Guerrero = hacerAlgo(g => g.copy(energia = (g.energia + cantidad).min(g.energiaMaxima)))

  def incrementarMaximo(cantidad: Int): Guerrero = hacerAlgo(g => g.copy(energiaMaxima = g.energiaMaxima + cantidad))

  def reducirMaximo(cantidad: Int): Guerrero =
    hacerAlgo(g =>
      if(g.energiaMaxima - cantidad > 0)
        g.copy(energia = g.energia.min(g.energiaMaxima - cantidad), energiaMaxima = (g.energiaMaxima - cantidad))
      else
        g.copy(energia = 1, energiaMaxima = 1))

  def morir: Guerrero = hacerAlgo(g => (g.especie match {
    case Fusionado(original, _) => original
    case _ => this
  }).reducirKi(g.energia))

  def quedarInconsciente: Guerrero = hacerAlgo(g => (g.especie match {
    case Fusionado(original, _) => original
    case saiyajin @ Saiyajin(_, _) => saiyajin.dejarDeSerSuperSaiyajin(g)
    case _ => this
  }).copy(estado = Inconsciente))

  def normalizar: Guerrero = hacerAlgo(g => g.copy(estado = Normal))

  def actualizarMunicion(nombre:String, nuevaCantidad: Int): Guerrero = {
    val arma: Option[Item] = inventario.get(nombre)
    if(!arma.isEmpty) arma.get match {
      case Arma(nombre, DeFuego(_)) => copy(inventario = inventario - nombre).copy(inventario = inventario + (nombre -> Arma(nombre, DeFuego(nuevaCantidad))))
      case _ => this
    }
    else this
  }

  def recibirExplosion(danio: Int): Guerrero = hacerAlgo(g =>
    if(g.especie == Namekusein) g.reducirKi(danio.min(g.energia - 1))
    else g.reducirKi(danio))

  def listarMovimientos: Map[String, Movimiento] = especie match {
    case Monstruo(_, _, movimientosDevorados) => movimientos ++ movimientosDevorados
    case _ => movimientos
  }

  def usarItem(item: String)(oponente:Guerrero): EstadoResultado = {
    if(estado != Muerto) {
      val resultadoBase = EstadoResultado(this, oponente)
      inventario.get(item).map({
          case i @ SemillaDelHermitanio => i.usar(resultadoBase)
          case _ if this.estado == Inconsciente => resultadoBase
          case Arma(_, DeFuego(municion)) if municion == 0 => resultadoBase
          case arma@Arma(nombre, DeFuego(municion)) => arma.usar(EstadoResultado(actualizarMunicion(nombre, municion - 1), oponente))
          case otro => otro.accion(resultadoBase)
      }).getOrElse(resultadoBase)
    }
    else EstadoResultado(this, oponente)
  }

  def tieneItem(item: String): Boolean = inventario.contains(item)

  def sabeMovimiento(movimiento: String): Boolean = listarMovimientos.contains(movimiento)

  def movimientoMasEfectivoContra(oponente: Guerrero)(criterio: EstadoResultado => tipos.RetornoCriterio): Option[Movimiento] = {
    val estadoInicial: EstadoResultado = EstadoResultado(this, oponente)
    val filtrarMovimientosValidos: ((String, Movimiento)) => Boolean =
      movimiento => criterio(movimiento._2.ejecutar(estadoInicial)) > 0
    val usarCriterio: ((String, Movimiento)) => tipos.RetornoCriterio =
      movimiento => criterio(movimiento._2.ejecutar(estadoInicial))
    val movimientosValidos = this.listarMovimientos.filter(filtrarMovimientosValidos)

    if(movimientosValidos.isEmpty)
      None
    else
     Some(movimientosValidos.maxBy[tipos.RetornoCriterio](usarCriterio)._2)
  }

  def pelearRound(movimiento: Movimiento)(oponente: Guerrero): EstadoResultado = {
    val primerAtaque: EstadoResultado = movimiento.ejecutar(EstadoResultado(this, oponente))
    primerAtaque.estadoOponente.contraAtacar(primerAtaque.estadoAtacante).map({
          case EstadoResultado(oponente, self) => EstadoResultado(self, oponente)
        }).getOrElse[EstadoResultado](primerAtaque)
  }

  def contraAtacar(oponente:Guerrero): Option[EstadoResultado] = {
    //El criterio rompe si el oponente se muere
    val criterioDeMasEnergia: EstadoResultado => tipos.RetornoCriterio = {case EstadoResultado(atacante, oponente) => atacante.energia / oponente.energia }
    movimientoMasEfectivoContra(oponente)(criterioDeMasEnergia).
      map(unMovimiento => unMovimiento.ejecutar(EstadoResultado(this, oponente)))
  }

  def planDeAtaqueContra(oponente: Guerrero, cantTurnos: Int)(criterio: EstadoResultado => tipos.RetornoCriterio): Option[List[Movimiento]] = {
    if(cantTurnos == 0) return Some(List[Movimiento]()) //Condicion de corte (y donde me canse de pensar)

    val movimientoMasEfectivo: Option[Movimiento] = this.movimientoMasEfectivoContra(oponente)(criterio)
    movimientoMasEfectivo match {
      case None => None
      case Some(movimiento) =>
        pelearRound(movimiento)(oponente).obtenerResultado match {
          case Terminada(_) => if(cantTurnos == 1) Some(List(movimiento)) else None
          case Peleando(atacante, oponente) =>
            atacante.planDeAtaqueContra(oponente, cantTurnos - 1)(criterio).map(l => List[Movimiento](movimiento) ++ l)
        }
    }
  }

  def pelearContra(oponente: Guerrero)(planDeAtaque: List[Movimiento]): Resultado = {
    if(planDeAtaque.isEmpty)
      Resultado(this, oponente)
    else {
      pelearRound(planDeAtaque.head)(oponente).obtenerResultado match {
        case Peleando(estadoAtacante, estadoOponente) => estadoAtacante.pelearContra(estadoOponente)(planDeAtaque.tail)
        case termino => termino
      }
    }
  }

}


sealed trait Especie {
  def unapply(arg: Guerrero): Option[Guerrero] = if(arg.especie == this) Some(arg) else None
}
case object Humano extends Especie
case class Saiyajin(tieneCola : Boolean = true,
                    estado: EstadoSaiyajin = SaiyajinNormal) extends Especie{

  require(estado != Mono || tieneCola, "Un saiyajin no puede ser mono sin tener cola")

  def cortarCola(guerrero: Guerrero): Guerrero = guerrero.especie match {
    case Saiyajin(true, Mono) => guerrero.reducirKi(guerrero.energia - 1).copy(especie = Saiyajin(false, SaiyajinNormal)).quedarInconsciente
    case Saiyajin(true, otro) => guerrero.reducirKi(guerrero.energia - 1).copy(especie = Saiyajin(false, otro))
    case _ => guerrero
  }

  def transformarEnMono(guerrero: Guerrero): Guerrero = guerrero.especie match {
    case Saiyajin(true, estado) if guerrero.tieneItem(FotoDeLuna.nombre) && estado != Mono =>{
      val sinSS = this.dejarDeSerSuperSaiyajin(guerrero)
      sinSS.incrementarMaximo(sinSS.energiaMaxima * 2).restaurar.hacerAlgo(_.copy(especie = Saiyajin(true, Mono)))
    }
    case _ => guerrero
  }

  def convertiseEnSuperSaiyajin(self: Guerrero): Guerrero = self.especie match {
    case Saiyajin(tieneCola, SaiyajinNormal) if self.energia > self.energiaMaxima / 2 =>
      self.hacerAlgo(g=> g.copy(especie = Saiyajin(tieneCola, SuperSaiyajin(g.energiaMaxima)))).incrementarMaximo(self.energiaMaxima * 4)
    case Saiyajin(tieneCola, SuperSaiyajin(energiaOriginal, nivel)) if self.energia > self.energiaMaxima / 2 =>
      self.hacerAlgo(g=> g.copy(especie = Saiyajin(tieneCola, SuperSaiyajin(energiaOriginal, nivel + 1)))).incrementarMaximo(energiaOriginal * 5)
    case _ => self
  }

  def dejarDeSerSuperSaiyajin(guerrero: Guerrero): Guerrero = guerrero.especie match {
    case Saiyajin(tieneCola, SuperSaiyajin(energiaOriginal, _)) => guerrero.reducirMaximo(guerrero.energiaMaxima - energiaOriginal).hacerAlgo(_.copy(especie = Saiyajin(tieneCola, SaiyajinNormal)))
    case _ => guerrero
  }
}
case object Androide extends Especie
case object Namekusein extends Especie
case class Monstruo(puedeDevorar: Guerrero => Boolean,
                    formaDeComer : (Guerrero, Map[String, Movimiento]) => Map[String, Movimiento],
                    movimientosDevorados : Map[String, Movimiento] = Map[String, Movimiento]()) extends Especie {

  def devorar(self: Guerrero, oponente: Guerrero): EstadoResultado =
    if(puedeDevorar(oponente) && self.energia > oponente.energia)
      EstadoResultado(self.copy(especie = this.copy(movimientosDevorados = formaDeComer(oponente, movimientosDevorados))),
              oponente.morir)
    else
      EstadoResultado(self, oponente)
}
case class Fusionado(original : Guerrero, amigo : Guerrero) extends Especie {

  def obtenerFusion: Guerrero = Guerrero("Fusion de " + original.nombre + " y " + amigo.nombre,
    original.energia + amigo.energia,
    original.energiaMaxima + amigo.energiaMaxima,
    this,
    original.movimientos ++ amigo.movimientos,
    original.inventario ++ amigo.inventario
    )
}


trait Item {
  val nombre: String
  val accion: tipos.Accion = res => res

  def usar(resultado: EstadoResultado): EstadoResultado = accion(resultado)
}
case class ItemBasico(nombre: String, override val accion: tipos.Accion = res => res) extends Item

case object SemillaDelHermitanio extends Item {
  val nombre: String = "Semilla del hermitaño"
  override val accion: tipos.Accion = res => res.copy(estadoAtacante = res.estadoAtacante.restaurarPorSemilla)
}
case class Arma(nombre: String, tipoArma: TipoArma) extends Item {
  override val accion: tipos.Accion = tipoArma.procesar(_)
}
case object FotoDeLuna extends Item {
  val nombre: String = "Foto de la luna"
}

case class EsferasDelDragon(cantidad: Int) extends Item {
  val nombre: String = "Esferas del dragon (" + cantidad + ")" //No me acuerdo como se interpolaban strings
}

trait Movimiento {
  val nombre: String
  val accion: tipos.Accion

  def ejecutar(resultado: EstadoResultado): EstadoResultado = if(resultado.estadoAtacante.sabeMovimiento(nombre)) accion(resultado) else resultado
}

case class UsarItem(item: Item) extends Movimiento {
  val nombre: String = "Usar " + item.nombre
  val accion: tipos.Accion = res => res.estadoAtacante.usarItem(item.nombre)(res.estadoOponente)
}

case class MovimientoSimple(nombre: String,
                      accion: tipos.Accion) extends Movimiento

case class Fusion(amigo: Guerrero) extends Movimiento {
  val nombre:String = "Fusion con " + amigo.nombre
  //require(amigo.especie == Humano || amigo.especie.isInstanceOf[Saiyajin] || amigo.especie == Namekusein, "Solo los humanos, saiyajins y namekuisein pueden fusionarse")
  val accion: tipos.Accion = res => (res.estadoAtacante, amigo) match {
    case (PuedeFusionarse(atacante), PuedeFusionarse(_)) => EstadoResultado(Fusionado(atacante, amigo).obtenerFusion, res.estadoOponente)
    case (_, _) => res
  }
}

case class Magia(nombre: String, hechizo: tipos.Accion) extends Movimiento {
  val accion: tipos.Accion = res => res.estadoAtacante match {
    case EspecieConMagia(_) => hechizo(res)
    case atacante if EspecieConMagia.tieneSieteEsferas(atacante) => hechizo(EstadoResultado(atacante.copy(inventario = atacante.inventario - "Esferas del dragon (7)"
), res.estadoOponente))
    case _ => res
  }
}

trait AtaqueDeEnergia extends Movimiento
{
  val danio: EstadoResultado => Int
  val efecto: Guerrero => Guerrero = g => g
  val accion: Accion = res =>
    if(res.estadoOponente.especie == Androide)
     EstadoResultado(efecto(res.estadoAtacante), res.estadoOponente.incrementarKi(danio(res)))
    else
     EstadoResultado(efecto(res.estadoAtacante), res.estadoOponente.reducirKi(danio(res)))


}

case class Onda(nombre:String, costo: Int) extends AtaqueDeEnergia {
  val danio: EstadoResultado => Int = res => res.estadoOponente.especie match {
    case _ if res.estadoAtacante.energia < costo => 0
    case Monstruo(_, _, _) => costo / 2
    case _ => costo * 2
  }

  override val efecto: Guerrero => Guerrero = _.hacerAlgo({g => if(g.energia > costo) g.reducirKi(costo) else g})
}

case object Genkidama extends AtaqueDeEnergia {
  val nombre: String = "Genkidama"
  override val danio: EstadoResultado => Int = res => if(res.estadoAtacante.turnosSiendoFajado > 0) math.pow(10, res.estadoAtacante.turnosSiendoFajado).toInt else 0
  override val efecto: Guerrero => Guerrero = {_.hacerAlgo(g => g)}
}

case class AtaqueFisico(nombre:String, accion: tipos.Accion) extends Movimiento


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
case class Peleando(estadoAtacante: Guerrero, estadoOponente: Guerrero) extends Resultado {
}
case class Terminada(ganador: Guerrero) extends Resultado
case class EstadoResultado(estadoAtacante: Guerrero, estadoOponente: Guerrero) {
  def afectarMasDebil(f: Guerrero => Guerrero): EstadoResultado =
    if(estadoAtacante.energia > estadoOponente.energia) EstadoResultado(estadoAtacante, f(estadoOponente))
    else EstadoResultado(f(estadoAtacante), estadoOponente)

  def obtenerResultado: Resultado = Resultado(estadoAtacante, estadoOponente)
}


sealed trait TipoArma {
  def procesar(res: EstadoResultado): EstadoResultado
}
case object Filosa extends TipoArma {
  override def procesar(res: EstadoResultado): EstadoResultado = {
    res.estadoOponente.especie match {
      case saiyajin @ Saiyajin(true, _) => EstadoResultado(res.estadoAtacante, saiyajin.cortarCola(res.estadoOponente))
      case _ => EstadoResultado(res.estadoAtacante, res.estadoOponente.reducirKi(res.estadoAtacante.energia * 100))
    }
  }
}
case class DeFuego(municion: Int) extends TipoArma {
  override def procesar(res: EstadoResultado): EstadoResultado = {
    val estadoAtacante: Guerrero = res.estadoAtacante
    res.estadoOponente match {
      case Humano(oponente) => EstadoResultado(estadoAtacante, oponente.reducirKi(20))
      case Namekusein(oponente) if oponente.estado == Inconsciente => EstadoResultado(estadoAtacante, oponente.reducirKi(10))
      case _ => res
    }

  }
}
case object Roma extends TipoArma {
  override def procesar(res: EstadoResultado): EstadoResultado = {
    if(res.estadoOponente.especie != Androide && res.estadoOponente.energia < 300)
      EstadoResultado(res.estadoAtacante, res.estadoOponente.quedarInconsciente)
    else
      res
  }
}

sealed trait EstadoSaiyajin
case object SaiyajinNormal extends EstadoSaiyajin
case object Mono extends EstadoSaiyajin
case class SuperSaiyajin(energiaOriginal: Int, nivel: Int = 1) extends EstadoSaiyajin {
  require(nivel > 0, "El nivel de super saiyajin debe ser mayor a cero")
}
