class Symbol
  def call(_something)
    true
  end

  def bindear(method_nombre, valor_variable, un_patron)
    un_patron.define_singleton_method(method_nombre.to_sym) { valor_variable }
  end

end

class Object

  def matches?(un_obj, &bloque_match)
    Matcher.new(un_obj, &bloque_match).matches?
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

  def list(elems, match_size = true)
    PatternMatching.new.list(elems, match_size)
  end

end

module Binder
  def bind(_un_obj)
    true
  end
end

module Compositor
  attr_accessor :hijos, :padre, :stack_patrones
  def initialize(matchers = nil)
    @hijos = matchers
    @stack_patrones ||= []
  end
  def cargarPadre(un_matcher)
    un_matcher.padre = self
    un_matcher
  end
  def is_a_matcher?(un_obj)
    un_obj.is_a?(Symbol) || un_obj.is_a?(PatternMatching)
  end
end

class PatternMatching
  include Compositor
  attr_accessor :patron, :bloque_patron

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

  def cargar_patron(un_patron, un_bloque_linea)

    self.patron = un_patron
    self.bloque_patron = un_bloque_linea
    self
  end


  def with(*matchers, &bloqueLinea)
    self.patron = matchers
    self.bloque_patron = bloqueLinea
    @stack_patrones.push(PatternMatching.new.cargar_patron(matchers, bloqueLinea))
    self
  end

  def otherwise(&bloqueLinea)
    self.patron = [:siempre_pasa]
    self.bloque_patron = bloqueLinea
    @stack_patrones.push(PatternMatching.new.cargar_patron(self.patron, bloqueLinea))
    self
  end

  def do_binding(un_obj)
    matchers.each{|unMatcher| unMatcher.bind(un_obj)}
  end

end

class Matcher
  attr_accessor :obj, :bloque_gral, :bloques
  def initialize(un_obj,&bloque_match)
    self.obj = un_obj
    self.bloque_gral = bloque_match
    self.bloques ||= []
  end

  def ganador(patrones, obj)
    primer_patron = patrones.first.patron.first
    if primer_patron.call(obj)
      patrones.first
    else
      ganador(patrones.drop(1), obj)
    end
  end

  def matches?
    pm = PatternMatching.new.instance_exec &bloque_gral

   el_patron_ganador = ganador(pm.stack_patrones,@obj)

    bloque = el_patron_ganador.bloque_patron

    el_patron_ganador.patron.first.instance_exec &bloque

  end
end

class ValueMatcher < PatternMatching
  include Binder
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
  include Binder
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
    self.list = alist
                    .map { |a_value|
      if is_a_matcher?(a_value)
        a_value
      else
        ValueMatcher.new(a_value)
      end
    }
    self.match_size = match_size
  end

  def bindear(variables, un_patron)

    variables.each do |a,b|
      if a.is_a?(Symbol)
      a.bindear(a, b, un_patron)
      end
    end

  end

  def call(list_to_compare)
    aux = list.zip list_to_compare
    result = false
    if match_size
      # TODO: no está bueno preguntar por symbol acá
      # DONE: Lo pregunto mas arriba para llamarlo polimórficamente
    result =   (list_to_compare.length == list.length) &&
        aux.none? { |_, b| b.nil? } &&
        (aux.all? { |elem_lista_1, elem_lista_2| elem_lista_1.call(elem_lista_2) })

    else
      # TODO: ojo con el codigo repetido
      # DONE: Reducido a una sola línea
     result = aux.all? { |elem_lista_1, elem_lista_2| elem_lista_1.call(elem_lista_2) || elem_lista_2.nil? }
    end

    if result
      bindear(aux, self)
    end
    result
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
