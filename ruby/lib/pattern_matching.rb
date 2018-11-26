
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
    list.all? {|e| list_to_compare.include?(e)}
    else
    list_to_compare.all? {|e| list.include?(e)}
    end
  end
end