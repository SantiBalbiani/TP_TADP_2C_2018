
import scala.language.postfixOps
// Requerido por enunciado Req#2
import MovimientosBasicos.prioridadAtaque

object Modelo {

  //=============== RESULTADO PELEA ============================================================================

  trait Resultado

  case class Ganador(unGuerrero: Guerrero) extends Resultado

  case class SiguenPeleando(peleadores: (Guerrero, Guerrero)) extends Resultado

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

    def tieneElItem(unItem: Item): Boolean = {
      inventario.contains(unItem)
    }

    def estaMuerto: Boolean = {
      estado match {
        case Muerto => true
        case _ => false
      }
    }

    def movimientoMasEfectivoContra(unGuerrero: Guerrero)(unCriterio: Criterio): Option[Movimiento] = {
//      if (!unGuerrero.estaMuerto) {
        // TODO cuidad: te estás olvidando de los movimientos que guardaste en la especie "Monstruo"
        // - los guerreros podrían tener un mensaje para retornar sus movimientos pero sigue siendo peligroso hacer pattern matching contra el atributo movimientos (porque son solo algunos, no todos)...
      if (movs.nonEmpty) {
      //val movsCalif: Option[Movimiento] =
        Option(
          // TODO cuidado el "Option(" no hace nada más que convertir un null en None!
          movs
            .map { unMov =>
              val (a, b) = unMov.ejecutarMov(this, unGuerrero)
              // TODO cuidado! el criterio tiene condiciones sobre si es positivo o negativo, el "abs" rompe ese contrato!
              // DONE: abs quitado
              (unCriterio(a, b), unMov)
            }
            // TODO cuidado, el maxBy tira exception si la lista es vacía
            // DONE: Pregunto si la lista es vacía antes
            .maxBy(_._1)._2
        )}else{
        None
      }
        //        movsCalif match {
        //          case Some((i, movimiento)) => if (i > 0) {
        //            Some(movimiento)
        //          } else {
        //            None
        //          }
        //          case None => None
        //        }
        // TODO esto es una forma similar de hacer la condicion anterior (pero más compacta)
       // movsCalif.filter(_._1 > 0).map(_._2)
//      } else {
//        None
//      }
    }

    def pelearRound(mov: Movimiento)(unOponente: Guerrero): (Guerrero, Guerrero) = {
      val despuesDe1erMov: (Guerrero, Guerrero) = mov.ejecutarMov(this, unOponente)
      val posibleMovimientoDelOponente: Option[Movimiento] = despuesDe1erMov._2.movimientoMasEfectivoContra(despuesDe1erMov._1)(prioridadAtaque)

      posibleMovimientoDelOponente match {
        case Some(movOponente) =>
          val resultRound: (Guerrero, Guerrero) = movOponente.ejecutarMov(despuesDe1erMov._2, despuesDe1erMov._1)
          (resultRound._2, resultRound._1)
        case None => despuesDe1erMov
      }
    }

    def plan(op: Guerrero, rounds: Int)(criterio: Criterio, movs: List[Movimiento] = List()): Option[List[Movimiento]] = {
      // TODO reimplementé el plan porque tenía dudas sobre un caso (andaba bien tu lógica)
      if (rounds == 0) {
        Some(movs)
      } else {
        movimientoMasEfectivoContra(op)(criterio).flatMap{ m =>
          val (g1, g2) = m.ejecutarMov(this, op)
          if (g1.estaMuerto) {
            None
          } else {
            g1.plan(g2, rounds - 1)(criterio, movs :+ m)
          }
        }
      }
    }

    def planDeAtaqueContra(unOponente: Guerrero, cantRounds: Int)(unCriterio: Criterio): Option[List[Movimiento]] = {
      // TODO aca salía mejor un flatmap
      movimientoMasEfectivoContra(unOponente)(unCriterio)
        .flatMap { unMovimiento =>
          val estadoP: (Guerrero, Guerrero) = pelearRound(unMovimiento)(unOponente)
          // TODO tiene que seguir aunque el otro esté muerto por la cantidad de movimientos
          if (cantRounds == 0 /*|| estadoP._2.estaMuerto*/ ) {
            Some(List[Movimiento]())
          } else if (estadoP._1.estaMuerto) {
            None
          } else {
            estadoP._1.planDeAtaqueContra(estadoP._2, cantRounds - 1)(unCriterio).map(movim =>
              List[Movimiento](unMovimiento) ++ movim
            )
          }
        }
    }

    def retornarResultado(guerreros:(Guerrero, Guerrero)): Resultado = {
      guerreros match{
        case (Guerrero(_, _, _, _, _, _, Muerto), Guerrero(_, _, _, _, _, _, Muerto)) => Ganador(guerreros._1)
        case (Guerrero(_, _, _, _, _, _, _), Guerrero(_, _, _, _, _, _, Muerto)) => Ganador(guerreros._1)
        case (Guerrero(_, _, _, _, _, _, Muerto), Guerrero(_, _, _, _, _, _, _)) => Ganador(guerreros._2)
        case (Guerrero(_, _, _, _, _, _, _), _) => SiguenPeleando(guerreros)
      }
    }

    def pelearContra(unOponente: Guerrero)(unPlan: List[Movimiento]): Resultado = {
      val plan = Option(unPlan)
      plan match {
        case Some(unPlan) =>
          if (unPlan.nonEmpty || (this.ki == 0) || (unOponente.ki == 0)) {
            val estado: (Guerrero, Guerrero) = pelearRound(unPlan.head)(unOponente)
            val planActualizado = unPlan.tail
            estado._1.pelearContra(estado._2)(planActualizado)
          } else {
            retornarResultado(this, unOponente)
          }
        case None => retornarResultado(this, unOponente)
      }
    }
  }

}
