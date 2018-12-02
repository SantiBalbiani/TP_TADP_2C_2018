class Symbol
  def call(_something)
    true
  end
end

class Object


  def matches?(un_obj, &bloque_match)
    PatternMatching.new.matches?(un_obj, &bloque_match)
  end

  def type(algo)
    PatternMatching.new.type(algo)
  end

  def duck(*algo)
    PatternMatching.new.duck(*algo)
  end

  def val(un_val)
    PatternMatching.new.val(un_val)
  end

  def list(*elems, match_size)
    PatternMatching.new.list(elems, match_size)
  end

end

module Compositor
  attr_accessor :hijos, :padre
  def initialize(matchers = nil)
    @hijos = matchers
  end
  def cargarPadre(un_matcher)
    un_matcher.padre = self
    un_matcher
  end
end

class PatternMatching
  include Compositor
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

  def AND(*matchers)
    a = AND_Matcher.new matchers
    cargarPadre(a)
  end

  def OR(*matchers)
    a = OR_Matcher.new matchers
    cargarPadre(a)
  end

  def NOT
    a = NOT_Matcher.new
    cargarPadre(a)
  end

  def matches?(un_obj, &bloque_matcher)

    a = Matcher.new(un_obj, &bloque_matcher)
    ??
  end

  def with(*matchers, &bloqueLinea)
    a = With_Matcher.new(matchers)
    ??
  end

  def otherwise
    ??
  end

  def do_binding
    ??
  end

  def matchea?
    ??
  end

end

class Matcher
  attr_accessor :un_obj, :bloque
def initialize(obj, &bloque_matcher)
  self.un_obj = obj
  self.bloque = bloque_matcher
end
end

class With_Matcher < PatternMatching
  include Compositor
end

class ValueMatcher < PatternMatching
  include Compositor
  attr_accessor :valor

  def initialize(aValue)
    self.valor = aValue
  end

  def call(value)
    value == valor
  end
end

class TypeMatcher < PatternMatching
  include Compositor
  attr_accessor :type

  def initialize(aType)
    self.type = aType
  end

  def call(value)
    value.is_a?(type)
  end
end

class ListMatcher < PatternMatching
  include Compositor
  attr_accessor :list, :match_size

  def initialize(alist, match_size)
    self.list = alist.map { |a_value|
      if a_value.is_a?(Symbol)
        a_value
      else
        ValueMatcher.new(a_value)
      end
    }
    self.match_size = match_size
  end

  def call(list_to_compare)
    aux = list.zip list_to_compare
    if match_size
      # TODO: no está bueno preguntar por symbol acá
      # DONE: Lo pregunto mas arriba para llamarlo polimorficamente
      (list_to_compare.length == list.length) &&
        aux.none? { |_, b| b.nil? } &&
        (aux.all? { |elem_lista_1, elem_lista_2| elem_lista_1.call(elem_lista_2) })

    else
      # TODO: ojo con el codigo repetido
      # DONE: Reducido a una sola línea
      aux.all? { |elem_lista_1, elem_lista_2| elem_lista_1.call(elem_lista_2) || elem_lista_2.nil? }
    end
  end
end

class DuckTypingMatcher < PatternMatching
  include Compositor
  attr_accessor :mensajes

  def initialize(*mensajes)
    self.mensajes = *mensajes
  end
  def call(anObject)
    # TODO: es mejor verificar con "respond_to?" porque podría responder al mensaje usando (por ejemplo) method missing
    # TODO Ojo que estás probando que todos los métodos no heredados del objeto estén incluidos en la lista que te pasaron, cuando lo que tenés que probar es que los que te pasaron los "tiene"/"entiende" el objeto
    # DONE: Ahora usa respond_to
    mensajes.flatten.all? do |un_mensaje| anObject.respond_to?(un_mensaje) end
  end
end

class AND_Matcher < PatternMatching
  include Compositor
  def call(algo)
    hijos.flatten.all? { |hijo| hijo.call(algo) } && padre.call(algo)
  end
end

class OR_Matcher < PatternMatching
  include Compositor
  def call(algo)
    hijos.flatten.all? { |hijo| hijo.call(algo) } || padre.call(algo)
  end
end

class NOT_Matcher < PatternMatching
  include Compositor
  def call(algo)
    !padre.call(algo)
  end
end
