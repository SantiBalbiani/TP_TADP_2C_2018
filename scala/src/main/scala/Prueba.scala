case class Guerrero(energia : Int,
                    energiaMaxima : Int,
                    especie : Especie,
                    movimientos : Map[String, Movimiento] = Map[String, Movimiento](),
                    inventario : Map[String, Item] = Map[String, Item](),
                    turnosSiendoFajado : Short = 0) {
  
  require(energia > 0, "La energia no puede ser negativa")
  require(energiaMaxima > energia, "La energia no puede superar el maximo")
  require(energiaMaxima > 0, "La energia maxima no puede ser negativa")
  require(turnosSiendoFajado >= 0, "turnosSiendoFajado debe ser positivo o 0")
  
  def modificarEnergia(delta : Int): Estado = Estado(copy(energia = (energia + delta).max(0).min(energiaMaxima)))

  def modificarEnergiaMaxima(delta : Int): Estado = Estado(copy(energiaMaxima = energiaMaxima + delta max 0))

  def agregarItem(nombre: String, item: Item): Estado = Estado(copy(inventario = inventario updated (nombre, item)))

  def agregarMovimiento(nombre: String, movimiento: Movimiento): Estado(copy(movimientos = movimientos updated (nombre, movimiento)))

  def movimientoMasEfectivoContra(oponente: Guerrero)(criterio: Resultado => Int): Option[(String, Movimiento)] =
      Option(movimientos.maxBy((_, movimiento) => criterio(movimiento.metodo(this, oponente)))).
      filter((_, movimiento) => criterio(movimiento.metodo(this, oponente)) > 0)

  def conseguirMovimiento(nombre: String): Movimiento = movimientos.get(nombre)

  def pelearRound(movimiento : String)(oponente : Guerrero): Resultado = {
      val resultadoPrimerMovimiento:Resultado = conseguirMovimiento(movimiento)(this, oponente)
      val respuestaOponente:Option[(String, Movimiento)] = resultadoPrimerMovimiento.estadoOponente.movimientoMasEfectivoContra(resultadoPrimerMovimiento.estadoAtacante)(_ - _)
  }
}


sealed trait Especie

case class Humano() extends Especie
case class Saiyajin(tieneCola : Boolean = true,
                    esMono : Boolean = false,
                    nivelSS : Short = 0) extends Especie{
  require(nivelSS >= 0, "El nivel de super saiyajin no puede ser negativo")
}
case class Androide() extends Especie
case class Namekuseins() extends Especie
case class Monstruo(formaDeComer : (Guerrero, Guerrero) => Guerrero,
                    movimientosDevorados : Seq[Movimiento] = Seq()) extends Especie
case class Fusionado(original : Guerrero, amigo : Guerrero) extends Especie


trait Item

case class Arma(accion : (Guerrero, Guerrero) => Resultado, municiones : Item) extends Item

case class SemillaHermitanio(accion : (Guerrero, Guerrero) => Guerrero) extends Item

case class Movimiento(metodo : (Guerrero, Guerrero) => Resultado,
                      tipoAtaque: Option[TipoAtaque] = None)


sealed trait Estado {
  def usarItem(item : Item): Estado
}
object Estado {
	def apply(guerrero : Guerrero): Estado = {
    if(guerrero.energia == 0) Muerto(guerrero)
    else Normal(guerrero)
  }
}

case class Normal(guerrero: Guerrero) extends Estado
case class Inconsciente(guerrero: Guerrero) extends Estado
case class Muerto(guerrero: Guerrero) extends Estado


sealed trait TipoAtaque

case object Energia extends TipoAtaque
case object Fisico extends TipoAtaque


case class Resultado(estadoAtacante: Estado, estadoOponente: Estado)
  override def usarItem(item: Item): Estado = if(guerrero.tieneItem(item)) item.efecto(guerrero) else this
}
case class Inconsciente(guerrero: Guerrero) extends Estado {
  override def usarItem(item: Item): Estado = (guerrero, item) match {
    case (guerrero, item @ Item("Semilla del hermitaño", _)) if guerrero.tieneItem(item) =>
          Normal(guerrero.copy(energia = guerrero.energiaMaxima))
    case (_, _) => this
  }
}
case class Muerto(guerrero: Guerrero) extends Estado {
  override def usarItem(item: Item): Estado = this
}


sealed trait TipoAtaque

case object Energia extends TipoAtaque
case object Fisico extends TipoAtaque
