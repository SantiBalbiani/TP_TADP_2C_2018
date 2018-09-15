class PatternFound < Exception
end

class PatternNotFound < Exception
end

# TODO hacer explícito a que clase le están definiendo el "matches?"
def matches? (obj, &b)
  begin
    PatternMatching.new(obj).instance_eval &b
  rescue PatternFound
    # TODO "matches?" tiene que retornar lo que retorne el bloque del pattern que matcheó
    #   (sino lo único que puede hacer el bloque son mutaciones)
  else
    raise PatternNotFound, "Reached end of pattern matching block"
  end
end

class PatternMatching
  def initialize (obj)
    @obj = obj
  end

  def val (a_value)
    Matcher.new { |obj| obj == a_value}
  end

  def type (a_type)
    Matcher.new { |obj| obj.is_a? a_type}
  end

  def duck (*methods)
    Matcher.new { |obj| methods.all?(proc { |method| obj.methods.include? method}) }
  end

  def list (a_list, match_size = true) #No me deja ponerle el ? a match_size? -- Mati
    # TODO también podrían hacer un module "Matcher" y preguntar con is_a?
    # TODO Aca está toda la lógica del "matcher de listas", es mejor reificar el concepto y crear una clase ListMatcher
    # TODO  (además, para minimizar el problema de los bindings, deberían delegar la responsabilidad a los matchers)
    # Es decir: pueden tener matchers sin bindings, matchers con bindings (actualmente solo symbolos) y matchers compuestos (actualmente la lista, el Not, Or y And) (revisen el patrón composite).
    # De esa manera siempre hablan contra un único matcher y si es compuesto él va hablar con sus matchers para juntar todos los valores.
    # TODO Recuerden que el binding solo debe ser aplicado si el patrón matchea (tengan cuidado con los ANDs, ORs y NOT)
    # Es decir: si una rama del arbol no matchea, no hay que aplicarle ningún binding.
    # (pueden pensar la composición de matchers como arboles, el root puede ser "true" pero eso no implica que todos sus hijos lo sean)
    bindings = a_list.map { |matcher|  matcher.bindings if matcher.respond_to? :bindings }
    pos = -1
    bindings = bindings.map do |binds|
      pos += 1
      binds.map { |binder| ListBinder.new(binder, pos) } if binds
    end
    bindings.compact!
    bindings.flatten!

    Matcher.new (bindings) do |obj|
      cur_element = -1
      obj.is_a? Array and (not match_size or a_list.length == obj.length) and
      a_list.all? do |element|
        cur_element += 1
        # TODO lo mismo que antes, revisen si pueden incluir un module "Matchers" o algo similar en vez de preguntar por combinators que no es tan específico como buscan.
        if element.class.included_modules.include? Combinators
         #Otra alternativa seria element.respond_to? call pero esa tiene el problema de incluir a los Proc
          element.call(obj[cur_element])
        else
          element == obj[cur_element]
        end
      end
    end
  end

  def with (*matchers, &b)
    if matchers.all?(proc { |matcher| matcher.call(@obj) })
      matchers.each { |matcher| matcher.do_bindings @obj, self }
      # TODO usando "call" en el bloque del "with" no están seteando el self
      # TODO   fue una casualidad que todos sus bloques de los "with" están creados
      # TODO   dentro del bloque al que le hacen instance_eval
      b.call
      raise PatternFound
    end
  end

  def otherwise (&b)
    b.call
    raise PatternFound
  end
end

module Combinators
  def concat_bindings (*matchers)
    new_bindings = matchers.map{ |matcher| matcher.bindings}
    new_bindings.flatten!.concat self.bindings
    new_bindings
  end

  # TODO borré los "clone" y la interfaz "pattern" de los matchers
  #   (el call es una llamada al "pattern" interno por lo que no es necesario exponerlo)
  def and (*matchers)
    new_bindings = concat_bindings(*matchers)
    # TODO "and" y "all?" es lógica repetida, agreguen directamente el matcher actual a la lista y usen solo "all?"
    Matcher.new(new_bindings) { |obj| call(obj) and matchers.all? (proc {|matcher| matcher.call obj }) }
  end

  def or (*matchers)
    new_bindings = concat_bindings(*matchers)
    # TODO lo mismo que para el "and"
    Matcher.new(new_bindings) { |obj| call(obj) or matchers.any? {|matcher| matcher.call obj } }
  end

  def not
    Matcher.new(bindings) { |obj| !call(obj) }
  end
end

class Matcher
  include Combinators

  attr_reader :bindings, :pattern

  # TODO salvo para las listas, los bindings siempre están vacíos. Refactorear la clase para dividir los tipos de matchers
  def initialize (bindings = [], &b)
    @bindings = bindings
    @pattern = b
  end

  def call (obj)
    @pattern.call obj
  end

  def do_bindings (obj, pat_mtc)
    bindings.each { |binder| binder.bind(pat_mtc, obj)}
  end
end

class Symbol
  include Combinators

  def call (obj)
    true
  end

  def do_bindings (obj, pat_mtc)
    # TODO lógica repetida con "Binder#bind"
    pat_mtc.define_singleton_method self do obj end
  end

  def bindings
    [Binder.new(self)]
  end
end

class Binder
  def initialize (name)
    @name = name
  end

  def bind (pat_mtc, obj)
    pat_mtc.define_singleton_method(@name) { obj }
  end
end

class ListBinder
  def initialize (binder, pos)
    @binder = binder
    @pos = pos
  end

  def bind (pat_mtc, obj)
    @binder.bind pat_mtc, obj[@pos]
  end
end
