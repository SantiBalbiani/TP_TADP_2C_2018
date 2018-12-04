class No_Hubo_Match < Exception
end

class Object
  def matches?(un_obj, &bloque_match)
    Matcher.matches?(un_obj, &bloque_match)
  end
end

module MatcherPostaPosta
  def AND(*matchers)
    # TODO el unshift pone el "self" adelante de la lista (para mandarle la lista de matchers y el matcher al que le mandaron el "and")
    AND_Matcher.new(*matchers.unshift(self))
  end

  def OR(*matchers)
    OR_Matcher.new(*matchers.unshift(self))
  end

  def NOT
    NOT_Matcher.new(self)
  end
=begin
  def call(valor)
    raise "Impl missing"
  end
=end

  def bindear(valor, contexto)
    # nothing
  end
end

class AlwaysTrueMatcher
  include MatcherPostaPosta

  def call(valor)
    true
  end
end

class Pattern
  def initialize(matcher, block)
    @matcher = matcher
    @block = block
 #   @contexto = {}  #No se usa
  end

  def call(valor)
    @matcher.call(valor)
  end

  def eval(valor)
    @matcher.bindear(valor, self)
    self.instance_eval &@block
  end
end

class Symbol
  include MatcherPostaPosta
  def call(_something)
    true
  end

  def bindear(valor_variable, un_patron)
    un_patron.define_singleton_method(self) { valor_variable }
  end
end



# TODO: estás mezclando el pattern matching (como contexto) con los matchers (los que se fijan si un valor matchea o no)
# Los matchers no necesitan entender "val", "type", etc.
# El contexto no necesita entender "AND", "OR", etc
# Fijate de pensar el Matcher como un Module y el Contexto como una clase (puede ser esta clase)
class PatternMatching
  attr_accessor :patterns

  def initialize
    @patterns = []
  end

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

  def with(*matchers, &bloqueLinea)
    # TODO los matchers los podés convertir en un único matcher "AND"
    @patterns << Pattern.new(AND_Matcher.new(*matchers), bloqueLinea)
  end

  def otherwise(&bloqueLinea)
    # TODO ojo que este patron tiene efecto (bindea "siempre_pasa" ... sería mejor un patron "siempre true" sin efecto)
    # self.patron = [:siempre_pasa]
    # self.bloque_patron = bloqueLinea
    # @stack_patrones.push(PatternMatching.new.cargar_patron(self.patron, bloqueLinea))
    @patterns << Pattern.new(AlwaysTrueMatcher.new, bloqueLinea)
  end
end

class Matcher

  def self.matches?(valor, &bloque_gral)
    # TODO dado que no tenés que pasarle parámetros, instance_eval hace lo mismo que instance_exec
    contexto = PatternMatching.new
    contexto.instance_eval &bloque_gral
    matching_pattern = contexto.patterns.find { |p| p.call(valor) }
    if matching_pattern.nil?
      raise No_Hubo_Match, 'No hubo Match para ninguna instrucción'
    else
      matching_pattern.eval(valor)
    end
  end
end

class ValueMatcher #< PatternMatching
  include MatcherPostaPosta

  attr_accessor :valor

  def initialize(aValue)
    self.valor = aValue
  end

  def call(value)
    value == valor
  end
end

class TypeMatcher #< PatternMatching
  include MatcherPostaPosta
  attr_accessor :type

  def initialize(aType)
    self.type = aType
  end

  def call(value)
    value.is_a?(type)
  end
end

class ListMatcher #< PatternMatching
  include MatcherPostaPosta
  attr_accessor :list, :match_size

  def initialize(alist, match_size)
    self.list = alist
                    .map { |a_value|
      if a_value.is_a? MatcherPostaPosta
        a_value
      else
        ValueMatcher.new(a_value)
      end
    }
    self.match_size = match_size
  end

  def bindear(variables, contexto)
    # TODO no soporta que el valor esté dentro de una condición nesteada
    # variables.each do |a|
    #   if a.is_a?(Symbol)
    #   a.bindear(a, b, un_patron)
    #   end
    # end
    list.zip(variables).each do |matcher, value|
      matcher.bindear(value, contexto)
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
      # TODO: Por más que sea una sola línea, el código repetido está cno la otra parte del if (la parte del "aux.all?...")
     result = aux.all? { |elem_lista_1, elem_lista_2| elem_lista_1.call(elem_lista_2) || elem_lista_2.nil? }
    end

    # if result
    #   bindear(aux, self)
    # end
    result
  end
end

class DuckTypingMatcher #< PatternMatching
  include MatcherPostaPosta
  attr_accessor :mensajes

  def initialize(*mensajes)
    self.mensajes = *mensajes
  end
  def call(anObject)
    # TODO: es mejor verificar con "respond_to?" porque podría responder al mensaje usando (por ejemplo) method missing
    # TODO Ojo que estás probando que todos los métodos no heredados del objeto estén incluidos en la lista que te pasaron, cuando lo que tenés que probar es que los que te pasaron los "tiene"/"entiende" el objeto
    # DONE: Ahora usa respond_to
    #
    # TODO: el flatten no era necesario, el *mensajes ya viene como una lista, lo necesitaste porque en los tests cuando pasaste una lista no usaste el *
    # Te camibé los tests para sean así: duck(*psyduck.methods)
    mensajes.all? do |un_mensaje| anObject.respond_to?(un_mensaje) end
  end
end

class AND_Matcher
  include MatcherPostaPosta

  def initialize(*matchers)
    @matchers = matchers
  end

  # TODO Podés hacer que el bind y el call queden delegados a los "hijos"
  def call(algo)
    @matchers.all? { |hijo| hijo.call(algo) } # && padre.call(algo)
  end

  def bindear(algo, contexto)
    @matchers.each { |m| m.bindear(algo, contexto) }
  end
end

class OR_Matcher
  include MatcherPostaPosta

  def initialize(*matchers)
    @matchers = matchers
  end

  def call(algo)
    @matchers.any? { |hijo| hijo.call(algo) }
  end

  def bindear(algo, contexto)
    @matchers.each { |m| m.bindear(algo, contexto) }
  end

end

class NOT_Matcher
  include MatcherPostaPosta
  def initialize(*matchers)
    @matchers = matchers
  end

  def call(algo)
    !@matchers.first.call(algo)
  end

  def bindear(algo, contexto)
    @matchers.each { |m| m.bindear(algo, contexto) }
  end

end
