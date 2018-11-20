import tipos._

import scala.language.postfixOps


object tipos{
  type Criterio = Function2[Guerrero, Guerrero, Int]
  type MovimientoEfecto = Function2[Guerrero, Guerrero, (Guerrero, Guerrero)]
  //type MovimientoEfectoConItem = Function3[Guerrero, Guerrero, Item, (Guerrero, Guerrero)]
  type MovimientosCalificados = List[(Int,Movimiento)]
  type PlanDeAtaque = Option[List[Movimiento]]
  type FormaDeDigerir = Function1[Guerrero, Guerrero]
  // Criterio establecido para el Requerimiento #2
  val mayorPuntosDeKi: Criterio = (g1: Guerrero, g2:Guerrero) => g2.ki
  val prioridadAtaque: Criterio = (g1: Guerrero, g2:Guerrero) => (g1.ki - g2.ki).abs

}

object movimientosBasicos{

  val dejarseFajar: Movimiento = Movimiento("dejarseFajar", (g1:Guerrero, g2:Guerrero) => (g1.copy(), g2.copy()) )

  val cargarKi: Movimiento = Movimiento("cargarKi",(g1:Guerrero, g2:Guerrero) =>
    g1.tipo match {
      case Androide(_) => (g1.copy(), g2.copy())
      case Sayajin(ssjLvl,_,_) if ssjLvl > 0 => (g1.copy(ki = g1.ki + 150*ssjLvl),g2.copy())
      case _ => (g1.copy(ki = g1.ki + 100), g2.copy())
    }
  )


  def usarItem(unItem:Item): Movimiento = Movimiento("usarItem", (g1: Guerrero, g2: Guerrero) =>
    if (g1.tieneElItem(unItem)){ unItem match {
      case Arma(Roma) if !g2.tipo.equals(Androide) => (g1.copy(), g2.copy(estado = Inconsciente))
      case Arma(Filosa) =>

        g2 match {
          case Guerrero(_, _, _, _, Sayajin(lvl, true, true), _, _) => (g1.copy(), g2.copy(ki = 1, tipo = Sayajin(lvl, false, false), estado = Inconsciente))
          case Guerrero(_, _, _, _, Sayajin(lvl, true, false), _, _) => (g1.copy(), g2.copy(ki = 1, tipo = Sayajin(lvl, false, false)))
          case _ => (g1.copy(), g2.copy(ki = g2.ki - (g1.ki / 100)))
        }
      case Arma(Fuego(muni)) if muni > 0 =>
        g2 match {
          case Guerrero(_, _, _, _, Humano, _, _) => (g1.copy(), g2.copy(ki = g2.ki - 20)) // Actualizar muni en g1
          case Guerrero(_, _, _, _, Namekusein, _, Inconsciente) => (g1.copy(), g2.copy(ki = g2.ki - 10)) //Igual q arriba
          case _ => (g1.copy(), g2.copy())
        }
      case SemillaHermitanio => (g1.copy(ki = g1.kiMax), g2.copy())

      case _ => (g1.copy(), g2.copy())
    }} else {
      (g1.copy(), g2.copy())
    })

  val comerseOponente: Movimiento = Movimiento("comerseOponente", ejecutarMov = (g1: Guerrero, g2: Guerrero) =>
    g1 match {
      case Guerrero(_, _, _, _, Monstruo(_, formaDeDigerir), _, _) => (formaDeDigerir(g2), g2.copy(estado = Muerto))
      case _ => (g1.copy(), g2.copy())
    })



}


trait Estado

case object Normal extends Estado
case object Inconsciente extends Estado
case object Muerto extends Estado



case class Movimiento(nombre:String, ejecutarMov: MovimientoEfecto)


trait Especie
trait Item
trait TipoArma

case object Roma extends TipoArma
case object Filosa extends TipoArma
case class Fuego(municion: Int) extends TipoArma

case object SemillaHermitanio extends Item
case class Arma(tipo: TipoArma) extends Item


case object Humano extends Especie
case object Namekusein extends Especie
case class Androide(battery:Int) extends Especie
case class Monstruo(habilidadesAdquiridas: List[Movimiento], formaDeDigerir: FormaDeDigerir) extends Especie
case class Sayajin(ssjLvl:Int, tieneCola: Boolean, esMono: Boolean) extends Especie




case class Guerrero(nombre: String, ki: Int, kiMax: Int, movs: List[Movimiento], tipo: Especie, inventario: List[Item], estado: Estado){

  def tieneElItem(unItem: Item): Boolean = {

    this.inventario.contains(unItem)

  }

  def movimientoMasEfectivoContra(unGuerrero:Guerrero)(unCriterio: Criterio): Movimiento = {

    val movsCalif: MovimientosCalificados =
      movs.map{unMov => ( unCriterio(unMov.ejecutarMov(this, unGuerrero)_1,unMov.ejecutarMov(this, unGuerrero)_2)  ,unMov) }
    movsCalif.maxBy(_._1)._2

  }



  def pelearRound(mov: Movimiento)(unOponente:Guerrero): (Guerrero, Guerrero) = {

    val despuesDe1erMov: (Guerrero, Guerrero) = mov.ejecutarMov(this, unOponente)

    val movOponente: Movimiento = despuesDe1erMov._2.movimientoMasEfectivoContra(this)(prioridadAtaque)

    val resultRound: (Guerrero, Guerrero) = movOponente.ejecutarMov(despuesDe1erMov._2.copy(), despuesDe1erMov._1.copy())

    (resultRound._2, resultRound._1)

  }


  def estaMuerto: Boolean = {

    estado match{
      case Muerto => true
      case _ => false
    }

  }
  // val rounds: List[Int] =  List.range(1, cantRounds)
  def planDeAtaqueContra(unOponente: Guerrero, cantRounds: Int)(unCriterio: Criterio): PlanDeAtaque = {
    var movimientos: Option[List[Movimiento]] = Some(List(movimientoMasEfectivoContra(unOponente)(unCriterio)))
    val mov: Movimiento = movimientoMasEfectivoContra(unOponente)(unCriterio)
    val estadoP: (Guerrero, Guerrero) = pelearRound(mov)(unOponente)
    if (estadoP._1.estaMuerto) return None
    if (estadoP._2.estaMuerto) return movimientos
    if (cantRounds >= 0)
      movimientos = planDeAtaqueContra(estadoP._2, cantRounds - 1)(unCriterio).map(movim => List[Movimiento](mov) ++ movim)
    movimientos
  }

    // var movMasEf: Movimiento = movimientoMasEfectivoContra(unOponente)(unCriterio)

    //var peleaResult: (Guerrero, Guerrero) = pelearRound(movMasEf)(unOponente)

}
