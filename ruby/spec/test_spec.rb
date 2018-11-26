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

  end

  it 'Combinación de Matcher de Lista con Matcher de Simbolo' do

    expect(PatternMatching.new.list([:uno,:dos,:tres,:cuatro]).call([1,2,3,4]))

  end

end