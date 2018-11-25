class VariableMatcher
  attr_accessor :variable
  def call(algo)
    true
  end
end

class ValueMatcher

  attr_accessor :valor

  def call(value)
    value == valor
  end
end

class TypeMatcher
  attr_accessor :type

  def call(value)
    value.class == type
  end
end