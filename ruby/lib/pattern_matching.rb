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

  def list (a_list, match_size = true) #No me deja ponerle el ? a match_size? -- Mati
    bindings = a_list.map do |matcher|
      if(matcher.respond_to? :bindings)
        matcher.bindings
      end
    end
    pos = -1
    bindings = bindings.map do |binds|
      pos += 1
      if(binds)
        binds.map do |binder|
          if(binder)
            ListBinder.new(binder, pos)
          end
        end
      end
    end
    bindings.compact!.flatten!

    Matcher.new (bindings) do |obj|
      cur_element = -1
      obj.is_a? Array and (!match_size or a_list.length == obj.length) and
          a_list.all? do |element|
            cur_element += 1
            if(element.class.include? Combinators)
              element.call(obj[cur_element])
            else
              element == obj
            end
          end
    end
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

module Combinators
  def concat_bindings (*matchers)
    new_bindings = matchers.map{ |matcher| matcher.bindings}
    new_bindings.flatten!.concat self.bindings
    new_bindings
  end

  def and (*matchers)
    cloned_pattern = self.pattern.clone
    new_bindings = concat_bindings(*matchers)
    Matcher.new(new_bindings) { |obj| cloned_pattern.call obj and matchers.all? {|matcher| matcher.call obj } }
  end

  def or (*matchers)
    cloned_pattern = self.pattern.clone
    new_bindings = concat_bindings(*matchers)
    Matcher.new(new_bindings) { |obj| cloned_pattern.call obj or matchers.any? {|matcher| matcher.call obj } }
  end

  def not
    cloned_pattern = self.pattern.clone
    Matcher.new(self.bindings) { |obj| !cloned_pattern.call obj }
  end
end

class Matcher
  include Combinators

  attr_reader :bindings, :pattern

  def initialize (bindings = [], &b)
    @bindings = bindings
    @pattern = b
  end

  def call (obj)
    @pattern.call obj
  end

  def do_bindings (obj, pat_mtc)
    self.bindings.each { |binder| binder.bind(pat_mtc, obj)}
  end
end

class Symbol

  include Combinators

  def call (obj)
    true
  end

  def do_bindings (obj, pat_mtc)
    pat_mtc.define_singleton_method self do obj end
  end

  def bindings
    [Binder.new(self)]
  end

  def pattern
    proc { true }
  end
end

class Binder
  def initialize (name)
    @name = name
  end

  def bind (pat_mtc, obj)
    pat_mtc.define_singleton_method (@name) { obj }
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