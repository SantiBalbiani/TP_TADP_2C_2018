require 'rspec'
require_relative '../lib/pattern_matching'

describe 'Pattern Matching Testing' do
  it 'el call de cualquier Matcher de Variable da true' do
    variableMatcher = VariableMatcher.new
    variableMatcher.variable = :eLoco

    expect(variableMatcher.call(:eLoco)).to eq(true)
  end

  it 'matcher de valor 5 con call 5 es verdadero' do

    matcherValor = ValueMatcher.new
    matcherValor.valor = 5

    expect(matcherValor.call(5)).to eq(true)

  end

  it 'matcher de valor 5 con call 4 es falso' do

    matcherValor = ValueMatcher.new
    matcherValor.valor = 4

    expect(matcherValor.call(5)).to eq(false)

  end

  it 'matcher de valor 5 con call "5" es falso' do

    matcherValor = ValueMatcher.new
    matcherValor.valor = 5

    expect(matcherValor.call("5")).to eq(false)

  end

  it 'matcher de valor "5" con call "5" es verdadero' do

    matcherValor = ValueMatcher.new
    matcherValor.valor = "5"

    expect(matcherValor.call("5")).to eq(true)

  end

  it 'matcher de tipo Integer es verdadero para call 5' do

    type = TypeMatcher.new
    type.type = Integer

    expect(type.call(5)).to eq(true)

  end

  it 'matcher de tipo Integer es falso para call "5"' do

    type = TypeMatcher.new
    type.type = Integer

    expect(type.call("5")).to eq(false)

  end

end