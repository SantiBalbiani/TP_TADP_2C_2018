
import scala.language.postfixOps
// Requerido por enunciado Req#2
import MovimientosBasicos.prioridadAtaque

object Modelo {
  //================= MOVIMIENTO ===============================================================================

  case class Movimiento(nombre: String, ejecutarMov: MovimientoEfecto)

  //================ TIPO DE ATAQUE ============================================================================

  trait TipoAtaque

  case object Fisico extends TipoAtaque
  case object Energia extends TipoAtaque

  //================= TIPOS DE ARMA =============================================================================

  trait TipoArma

  case object Roma extends TipoArma

  case object Filosa extends TipoArma

  case class Fuego(municion: Int) extends TipoArma

  //=========================== ITEMS ============================================================================

  trait Item

  case object SemillaHermitanio extends Item

  case class Arma(tipo: TipoArma) extends Item

  //=========================== ESPECIES DE GUERREROS ============================================================
  trait Especie

  case object Humano extends Especie

  case object Namekusein extends Especie

  case class Androide(battery: Int) extends Especie

  case class Monstruo(habilidadesAdquiridas: List[Movimiento], formaDeDigerir: FormaDeDigerir) extends Especie

  case class Sayajin(ssjLvl: Int, tieneCola: Boolean, esMono: Boolean) extends Especie

  //============================= ESTADO DE LOS GUERREROS ==========================================================

  trait Estado

  case object Normal extends Estado

  case object Inconsciente extends Estado

  case object Muerto extends Estado

  //=================================== TIPOS =====================================================================

  type Criterio = Function2[Guerrero, Guerrero, Int]
  type MovimientoEfecto = Function2[Guerrero, Guerrero, (Guerrero, Guerrero)]
  type MovimientosCalificados = List[(Int, Movimiento)]
  type PlanDeAtaque = Option[List[Movimiento]]
  type FormaDeDigerir = Function2[Guerrero, List[Movimiento], Guerrero]

  //================================================================================================================

  case class Guerrero(nombre: String,
                      ki: Int,
                      kiMax: Int,
                      movs: List[Movimiento],
                      tipo: Especie,
                      inventario: List[Item],
                      estado: Estado) {
    require(ki >= 0, "no puede tener ki negativo")
    require(ki <= kiMax, "no puede tener mas ki que su maximo")
    //require((estado == Muerto && ki == 0) || (estado != Muerto && ki > 0),"Si no tiene ki esta muerto, y si tiene ki esta vivo")

    def actualizarEstado: Guerrero = {
      if (ki <= 0){
        this.copy(estado = Muerto)
      }
      this
    }

    def tieneElItem(unItem: Item): Boolean = {
      this.inventario.contains(unItem)
    }

    def estaMuerto: Boolean = {
      estado match {
        case Muerto => true
        case _ => false
      }
    }

    // TODO cuidado, el movimiento más efectivo podría no existír!
    // DONE: Si no existe movimiento mas efectivo retorna NONE (Santi)
    def movimientoMasEfectivoContra(unGuerrero: Guerrero)(unCriterio: Criterio): Option[Movimiento] = {
      // TODO no hace falta el tipo "MovimientosCalificados" (solo se usa acá)
      // DONE: Borrado (Santi)
      if (!unGuerrero.estaMuerto) {
        val movsCalif: Option[(Int, Movimiento)] = Option(movs.map { unMov =>
          (unCriterio(unMov.ejecutarMov(this, unGuerrero)._1, unMov.ejecutarMov(this, unGuerrero)._2).abs, unMov)
        }.maxBy(_._1))
        movsCalif match {
          case Some(movCalificado) => if (movCalificado._1 > 0) {
            Option(movCalificado._2)
          } else {
            None
          }
          case None => None
        }
      } else {
        None
      }
      // TODO si el criterio es negativo el movimiento no vale
      //DONE: Retorna un NONE en caso que sea negativo (Santi)
    }

    def pelearRound(mov: Movimiento)(unOponente: Guerrero): (Guerrero, Guerrero) = {

      val despuesDe1erMov: (Guerrero, Guerrero) = mov.ejecutarMov(this, unOponente)
      // TODO: ojo que estás usando "this" para el movimiento más efectivo (deberías usar el guerrero como quedó después del movimiento)
      // DONE (Santi)

      val posibleMovimientoDelOponente: Option[Movimiento] = despuesDe1erMov._2.movimientoMasEfectivoContra(despuesDe1erMov._1)(prioridadAtaque)

      posibleMovimientoDelOponente match{
        case Some(movOponente) =>
        val resultRound: (Guerrero, Guerrero) = movOponente.ejecutarMov(despuesDe1erMov._2.copy(), despuesDe1erMov._1.copy())
        (resultRound._2, resultRound._1)
        case None => despuesDe1erMov
      }

    }




    def planDeAtaqueContra(unOponente: Guerrero, cantRounds: Int)(unCriterio: Criterio): PlanDeAtaque = {
      // TODO se te va a complicar más pensar algo recursivo y a su vez mutable
      // var movimientos: Option[List[Movimiento]] = Some(List(movimientoMasEfectivoContra(unOponente)(unCriterio)))

      // Esto devuelve un Option (Santi)
      // Englobar todo con un match que devuelva None si el primer mov determinado es None (Santi)
      val mov: Option[Movimiento] = movimientoMasEfectivoContra(unOponente)(unCriterio)
      // val mov: Option[Movimiento] = movimientoMasEfectivoContra(unOponente)(unCriterio) // (Santi)
      // if mov is Some // (Santi)
      mov match{
        case Some(unMovimiento) =>

          val estadoP: (Guerrero, Guerrero) =   pelearRound(unMovimiento)(unOponente)
          if (estadoP._1.estaMuerto) {
            None
          } else if (estadoP._2.estaMuerto) {
            // movimientos (lo reemplazé por la definición) => fijate que ahora se nota que hay un bug,
            // si el otro está muerto me da una lista de 1 movimiento)
            Some(List(movimientoMasEfectivoContra(unOponente)(unCriterio)).map{Some_ => unMovimiento})
          } else if (cantRounds >= 0) {
            planDeAtaqueContra(estadoP._2, cantRounds - 1)(unCriterio).map(movim =>
              List[Movimiento](unMovimiento) ++ movim)
          }
          else {
            Some(List[Movimiento]())
          }
        case None => None
      }
    }

      // TODO evitá hacer return, preferí usar "else"


    // TODO la única forma de estar seguro que el algoritmo funciona es haciendo tests (ejemplos de casos de tests):
    // - si yo estoy muerto
    // - si el otro está muerto
    // - si el plan que estoy armando con este criterio me deja muerto antes de N rounds
    // - si no puedo conseguir movimientos para los N rounds
    // - si pido N rounds tengo que tener N movimientos


    def pelearContra(unOponente: Guerrero)(unPlan: List[Movimiento]): (Guerrero, Guerrero) = {

      val plan = Option(unPlan)
      plan match{
        case Some(unPlan) =>
          if (unPlan.nonEmpty) {
            val estado: (Guerrero,Guerrero) = pelearRound(unPlan.head)(unOponente)
            val planActualizado = unPlan.tail
            estado._1.pelearContra(estado._2)(planActualizado)
            }else{
            (this, unOponente)}
        case None => (this, unOponente)
      }
    }
  }
}
