class Symbol
  def call(something)
    true
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

end


class ValueMatcher

  attr_accessor :valor

  def initialize(aValue)
    self.valor = aValue
  end

  def call(value)
    value == valor
  end

end

class TypeMatcher
  attr_accessor :type

  def initialize(aType)
    self.type = aType
  end

  def call(value)
    value.is_a?(type)
  end

end

class ListMatcher
  attr_accessor :list, :match_size

  def initialize(alist, match_size)

    self.list = alist
    self.match_size = match_size

  end

  def call(list_to_compare)

    if match_size

      # TODO no está bueno preguntar por symbol acá

      self.list.sort == list_to_compare.sort

=begin
      if list.all? {|e| e.is_a?(Symbol)}
        list.all? {|e| e.call('something')}
      else
        list.all? {|e| list_to_compare.include?(e)}
      end
=end

    else
      # TODO ojo con el codigo repetido
      if list_to_compare.all? {|e| e.is_a?(Symbol)}
        list_to_compare.all? {|e| e.call('something')}
      else
        list_to_compare.all? {|e| list.include?(e)}
      end
    end
  end
end

class DuckTypingMatcher

  attr_accessor :mensajes

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