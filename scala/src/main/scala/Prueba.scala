import scala.util.Try

object tipos{
  type Accion = Resultado => Resultado
}

case class Guerrero(energia : Int,
                    energiaMaxima : Int,
                    especie : Especie,
                    movimientos : Map[NombreMovimiento, Movimiento] = Map[NombreMovimiento, Movimiento](),
                    inventario : Map[Item, Movimiento] = Map[Item, Movimiento](),
                    turnosSiendoFajado : Short = 0,
                    estado : Estado = Normal) {
  
  require(energia > 0, "La energia no puede ser negativa")
  require(energiaMaxima > energia, "La energia no puede superar el maximo")
  require(energiaMaxima > 0, "La energia maxima no puede ser negativa")
  require(turnosSiendoFajado >= 0, "turnosSiendoFajado debe ser positivo o 0")
  require(energia == 0 && estado == Normal, "El guerrero no puede estar bien sin energia")

  def movimientoMasEfectivoContra(oponente: Guerrero)(criterio: Resultado => Int): Option[(NombreMovimiento, Movimiento)] = {
    val estadoInicial: Resultado = Resultado(this, oponente)
    val filtrarMovimientosValidos: ((NombreMovimiento, Movimiento)) => Boolean =
      (movimiento) => criterio(movimiento._2.accion(estadoInicial)) > 0
    val usarCriterio: ((NombreMovimiento, Movimiento)) => Int =
      (movimiento) => criterio(movimiento._2.accion(estadoInicial))

    Try(this.movimientos.filter(filtrarMovimientosValidos).maxBy[Int](usarCriterio)).toOption
  }

  def pelearRound(movimiento: Movimiento)(oponente: Guerrero): Resultado = {
    //Este criterio solo busca la mayor ventaja
    val criterioDeMasEnergia: Resultado => Int = {case Resultado(atacante, oponente) => atacante.energia - oponente.energia}
    val primerAtaque: Resultado = movimiento.accion(Resultado(this, oponente))
    primerAtaque.estadoOponente.movimientoMasEfectivoContra(primerAtaque.estadoAtacante)(criterioDeMasEnergia).
      map(unMovimiento => unMovimiento._2.accion(primerAtaque)).getOrElse[Resultado](primerAtaque)
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
  def unapply(arg: Guerrero): Option[Guerrero] = if(arg.especie.getClass == this.getClass) Some(arg) else None
}
case class Humano() extends Especie
case class Saiyajin(tieneCola : Boolean = true,
                    esMono : Boolean = false,
                    nivelSS : Short = 0) extends Especie{
  require(nivelSS >= 0, "El nivel de super saiyajin no puede ser negativo")
  require((!esMono || tieneCola), "Un saiyajin no puede ser mono sin tener cola")
}
case class Androide() extends Especie
case class Namekuseins() extends Especie
case class Monstruo(formaDeComer : (Guerrero, Guerrero) => Guerrero,
                    movimientosDevorados : Seq[Movimiento] = Seq()) extends Especie
case class Fusionado(original : Guerrero, amigo : Guerrero) extends Especie


trait Item
case object SemillaHermitanio extends Item
case object FotoLuna extends Item
case class Arma(municion: Int) extends Item
case class EsferaDelDragon(cantEstrallas: Short) extends Item


trait NombreMovimiento
case class Movimiento(accion: tipos.Accion,
                      tipoAtaque: Option[TipoAtaque] = None)


sealed trait Estado {
  def unapply(arg: Guerrero): Option[Guerrero] = if(arg.estado == this) Some(arg) else None
}
case object Normal extends Estado
case object Inconsciente extends Estado
case object Muerto extends Estado


case class Resultado(estadoAtacante: Guerrero, estadoOponente: Guerrero)


sealed trait TipoAtaque
case object Energia extends TipoAtaque
case object Fisico extends TipoAtaque
