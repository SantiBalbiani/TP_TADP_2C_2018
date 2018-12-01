class Symbol
  def call(something)
    true
  end
end

class Compositor

  attr_accessor :hijos

  def initialize(matchers)
    @hijos = matchers
  end

  def agregar(hijo)
    @hijos.push(hijo)
    hijo.padre = this
  end

end

class PatternMatching

  def val(unVal)
    ValueMatcher.new(unVal)
  end

  def type(aType)
    TypeMatcher.new(aType)
  end

  def list(aList, match_size = true)
    ListMatcher.new(aList, match_size)
  end

  def duck(*messages)
    DuckTypingMatcher.new(*messages)
  end

  def AND(*hijos)
    AND_Matcher.new hijos
  end

end


class ValueMatcher

  attr_accessor :valor, :padre

  def initialize(aValue)
    self.valor = aValue
  end

  def call(value)
    value == valor
  end

  def AND(*cosas)

    PatternMatching.new.AND(*cosas)

  end

end

class TypeMatcher
  attr_accessor :type, :padre

  def initialize(aType)
    self.type = aType
  end

  def call(value)
    value.is_a?(type)
  end

end

class ListMatcher
  attr_accessor :list, :match_size, :padre

  def initialize(alist, match_size)

    self.list = alist.map {|a_value|
      if a_value.is_a?(Symbol)
        a_value
      else
        ValueMatcher.new(a_value)
      end}
    self.match_size = match_size

  end

  def call(list_to_compare)

    aux = list.zip list_to_compare

    if match_size

      # TODO no está bueno preguntar por symbol acá
      # DONE: Lo pregunto mas arriba para llamarlo polimorficamente
      (list_to_compare.length == list.length) &&
          aux.none? { |_, b| b.nil?} &&
          (aux.all? { |elem_lista_1, elem_lista_2| elem_lista_1.call(elem_lista_2)})

    else
      # TODO ojo con el codigo repetido
      # DONE: Reducido a una sola línea

      aux.all? {|elem_lista_1, elem_lista_2| elem_lista_1.call(elem_lista_2) || elem_lista_2.nil?}

    end
  end
end

class DuckTypingMatcher

  attr_accessor :mensajes, :padre

  def initialize(*mensajes)
    self.mensajes = *mensajes
  end

  def call(anObject)

    # TODO es mejor verificar con "respond_to?" porque podría responder al mensaje usando (por ejemplo) method missing
    # TODO Ojo que estás probando que todos los métodos no heredados del objeto estén incluidos en la lista que te pasaron, cuando lo que tenés que probar es que los que te pasaron los "tiene"/"entiende" el objeto
    # DONE: Ahora usa respond_to
    los_mensajes = mensajes.flatten
    los_mensajes.all? do |un_mensaje|
      anObject.respond_to?(un_mensaje)
    end
  end
end


class AND_Matcher < Compositor
  attr_accessor :padre

   def call(algo)
    super.hijos.all? {|hijo| hijo.call(algo)} && padre.call(algo)
  end

end