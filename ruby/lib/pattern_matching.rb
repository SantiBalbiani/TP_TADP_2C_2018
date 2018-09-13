class PatternFound < Exception
end

class PatternNoFound < Exception
end

def matches? (obj, &b)
  begin
    PatternMatching.new(obj).instance_eval &b
  rescue PatternFound
  else
    raise PatternNoFound, "Reached end of pattern matching block"
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

  def with (*matchers, &b)
    if(matchers.all?(proc { |matcher| matcher.call(@obj) }))
      matchers.each { |matcher| matcher.do_bindings @obj, self }
      b.call
      raise PatternFound
    end
  end

  def otherwise (&b)
    b.call
    raise PatternFound
  end
end

class Matcher
  attr_reader :bindings
  def initialize (bindings = [], &b)
    @bindings = bindings
    @pattern = b
  end

  def call (obj)
    @pattern.call obj
  end

  def do_bindings (obj, pat_mtc)
    self.bindings.each { |bind| pat_mtc.define_singleton_method bind { obj } }
  end

  def or (*matchers)
    cloned_pattern = @pattern.clone
    new_bindings = matchers.map{ |matcher| matcher.bindings}
    new_bindings.flatten!.concat self.bindings
    Matcher.new(new_bindings) { |obj| cloned_pattern.call obj or matchers.any? {|matcher| matcher.call obj } }
  end

  def and (*matchers)
    cloned_pattern = @pattern.clone
    new_bindings = matchers.map{ |matcher| matcher.bindings}
    new_bindings.flatten!.concat self.bindings
    Matcher.new(new_bindings) { |obj| cloned_pattern.call obj and matchers.all? (proc {|matcher| matcher.call obj }) }
  end

  def not
    cloned_pattern = @pattern.clone
    Matcher.new (self.bindings) { |obj| ! cloned_pattern.call obj }
  end
end

class Symbol
  def call (obj)
    true
  end

  def do_bindings (obj, pat_mtc)
    pat_mtc.define_singleton_method self do obj end
  end

  def bindings
    [self]
  end

  def or (*matchers)
    new_bindings = matchers.map { |matcher| matcher.bindings}
    new_bindings.flatten!.concat self.bindings
    Matcher.new (new_bindings) { |obj| true }
  end

  def and (*matchers)
    new_bindings = matchers.map{ |matcher| matcher.bindings}
    new_bindings.flatten!.concat self.bindings
    Matcher.new (new_bindings) { |obj| matchers.all? (proc {|matcher| matcher.call obj }) }
  end

  def not
    Matcher.new([self]) { |obj| false }
  end
end