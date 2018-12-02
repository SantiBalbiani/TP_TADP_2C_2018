require 'rspec'
require_relative '../lib/pattern_matching'

describe 'Matchers Básicos - Simbolo - Valor - Tipo - Lista' do
  it 'el call de cualquier Matcher de Variable da true' do
    expect(:algo.call('cualquierCosa')).to eq(true)
  end

  it 'matcher de valor 5 con call 5 es verdadero' do
    expect(PatternMatching.new.val(5).call(5)).to eq(true)
  end

  it 'matcher de valor 5 con call 4 es falso' do
    expect(PatternMatching.new.val(5).call(4)).to eq(false)
  end

  it 'matcher de valor 5 con call "5" es falso' do
    expect(PatternMatching.new.val(5).call("5")).to eq(false)
  end

  it 'matcher de valor "5" con call "5" es verdadero' do
    expect(PatternMatching.new.val("5").call("5")).to eq(true)
  end

  it 'matcher de tipo Integer es verdadero para call 5' do
    expect(PatternMatching.new.type(Integer).call(5)).to eq(true)
  end

  it 'matcher de tipo Integer es falso para call "5"' do
    expect(PatternMatching.new.type(Integer).call("5")).to eq(false)
  end

  it 'matcher de lista con match_zise y con su misma lista' do
    expect(PatternMatching.new.list([1, 2, 3, 4]).call([1,2,3,4])).to eq(true)
  end

  it 'matcher de lista con match_size y su misma lista menos un elemento' do
    expect(PatternMatching.new.list([1,2,3,4]).call([1,2,3])).to eq(false)
  end

  it 'matcher de lista sin match_size y su misma lista menos un elemento' do
    expect(PatternMatching.new.list([1,2,3,4], false).call([1,2,3])).to eq(true)

    # TODO ojo que el alg. de las listas está mal
    # DONE: Corregido
    end
    it 'test list 1' do
    expect(PatternMatching.new.list([2,1,3,4], false).call([1,2,3])).to eq(false)
    end
    it 'test list 2 ' do
    expect(PatternMatching.new.list([2,1,3,4], true).call([1,2,3])).to eq(false)
    end

  it 'test list 3' do
    expect(PatternMatching.new.list([1,2,3], true).call([1,2,3,4])).to eq(false)
  end
  it 'test list 4' do
    expect(PatternMatching.new.list([1,2,3], false).call([1,2,3,4])).to eq(true)
  end

  it 'Combinación de Matcher de Lista con Matcher de Simbolo' do
    expect(PatternMatching.new.list([:uno,:dos,:tres,:cuatro]).call([1,2,3,4])).to eq(true)
  end
end

describe 'Duck Typing' do
  psyduck = Object.new
  def psyduck.cuack
    'psy..duck?'
  end
  def psyduck.fly
    '(headache)'
  end

  class Dragon
    def fly
      'do some flying'
    end
  end
  a_dragon = Dragon.new




  it 'Conoce los mensajes, es un Pato' do
expect(PatternMatching.new.duck(psyduck.methods).call(psyduck)).to eq(true)
  end

  it 'Un dragon no sabe decir cuack' do
    expect(PatternMatching.new.duck(psyduck.methods(false)).call(a_dragon)).to eq(false)
  end
end

describe 'Matcher AND' do
  it 'prueba simple AND' do

    a = PatternMatching.new.val(5).AND(type(Integer),duck(:+))
          expect(a.call(5)).to eq(true)
  end

  it '2da prueba simple AND' do

    a = PatternMatching.new.val(4).AND(type(Integer),duck(:+))
    expect(a.call(5)).to eq(false)
  end
end

describe 'Matcher OR' do

  it 'prueba Simple OR' do

   a = PatternMatching.new.val(4).OR(type(Integer),duck(:+))

   expect(a.call(5)).to eq(true)
  end

end

describe 'Matcher NOT' do

  it 'prueba Simple NOT' do

    a = PatternMatching.new.val(4).NOT
    expect(a.call(5)).to eq(true)
  end

end
