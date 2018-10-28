


case class Movimiento(nombre:String, 
                      condicion: Guerrero => Boolean, //Evaluamos si el guerrero va a poder hacer el movimiento
                      efectoUsuario: Guerrero => Guerrero,
                      efectoOponente: Guerrero => Guerrero)

object Movimientos {
  val dejarseFajar : Movimiento = Movimiento("dejarse fajar", (_) => true, ???, ???)
  val cargarKi: Movimiento = Movimiento("cargarKi", (_)=> true, ???, ???)
  val usarItem: Movimiento = Movimiento("usarItem", (_)=> true, ???, ???)
  val comerseAlOponente: Movimiento = Movimiento("usarItem", (_)=> true, ???, ???)
  val atacar: Movimiento = Movimiento("atacar", (_)=> true, ???, ???)
  //...and so on
}

case class Ataque(unAtacante: Guerrero, unAtacado: Guerrero)

object Ataques {
  val muchosGolpesNinja: Movimiento = ???
}

case class Item(nombre: String, guerrero: Guerrero){
 
}



                    
  case class Guerrero(nombre: String, 
                    ki: Int, 
                    inventario: Seq[Item],
                    especie : Especie, 
                    movimientos: Set[Movimiento]
                    ) {
    def tieneItem(unItem:Item) {
    var result = true
  }
    
  }            
                    
  
  


trait Especie {
  //Para poder escribir un patron facilmente que diga que especie es
  def unapply(guerrero:Guerrero) : Option[Guerrero] = if(guerrero.especie == this) Some(guerrero) else None
  
}

case class Humano() extends Especie
case class Sayajin(tieneCola:Boolean) extends Especie
case class Androide() extends Especie
case class Namekusein() extends Especie
case class Monstruo(movimientosDigeridos: Set[Movimiento]) extends Especie
case class Fusionado(original:Guerrero, amigo:Guerrero) extends Especie


object TesteoLoco extends App {
  

  
}