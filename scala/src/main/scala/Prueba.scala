case object Prueba {

  def materia: String = "tadp"

}

case class Guerrero(energia : Int,
                    energiaMaxima : Int,
                    especie : Especie,
                    movimientos : List[Movimiento] = List(),
                    inventario : List[Item] = List(),
                    turnosSiendoFajado : Short = 0) {
  def tieneItem(item: Item): Boolean = inventario.contains(item)

}


sealed trait Especie
object Especie {
  //def unapply(arg: Guerrero): Option[Guerrero] = if(arg.especie.getClass == this.getClass) Some(arg) else None
}

case class Humano() extends Especie
case class Saiyajin(tieneCola : Boolean = true,
                    esMono : Boolean = false,
                    nivelSS : Short = 0) extends Especie
case class Androide() extends Especie
case class Namekuseins() extends Especie
case class Monstruo(formaDeComer : Guerrero => Guerrero => Guerrero,
                    movimientosDevorados : Seq[Movimiento] = Seq()) extends Especie
case class Fusionado(original : Guerrero, amigo : Guerrero) extends Especie


case class Item(nombre : String, efecto : Guerrero => Estado)


case class Movimiento(nombre : String,
                      metodo : Guerrero => Guerrero => Guerrero,
                      tipoAtaque: Option[TipoAtaque] = None)


sealed trait Estado {
  def usarItem(item : Item): Estado
}
object Estado {
  //def unapply(arg: Estado): Option[Guerrero] = if(arg.getClass == this.getClass) Some(arg.guerrero) else None

}

case class Normal(guerrero: Guerrero) extends Estado {
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