describe Prueba do
  let(:prueba) { Prueba.new }

  describe '#materia' do
    it 'debería pasar este test' do
      expect(prueba.materia).to be :tadp
    end
  end
end

describe PatternMatching do

  describe 'Test unitarios de Pattern Matching' do

    describe 'matcher de variable' do

      it 'devuelve siempre true' do
        expect(:a.call(2)).to be true
        expect(:a.call("Testing")).to be true
      end
    end

    describe 'matcher de valor' do
      it 'devuelve true si coinciden los valores' do
        expect(val(5).call(5)).to be true
        expect(val('5').call('5')).to be true
      end

      it 'devuelve false si no coinciden los valores' do
        expect(val(5).call(4)).to be false
        expect(val(5).call('5')).to be false
        expect(val('5').call('4')).to be false
      end
    end

    describe 'matcher de tipo' do
      it 'devuelve true si es del tipo correcto' do
        expect(type(Integer).call(5)).to be true
        expect(type(Symbol).call(:a)).to be true
      end

      it 'devuelve false si no es del tipo correcto' do
        expect(type(Integer).call('5')).to be false
        expect(type(Symbol).call("a")).to be false
      end
    end

    describe 'matcher de lista' do
      it 'da true si lista es la misma' do
        una_lista = [1, 2, 3, 4]
        expect(list(una_lista).call(una_lista)).to be true
        expect(list(una_lista, false).call(una_lista)).to be true
      end

      it 'da false si coiciden los tamaños pero no los elementos' do
        una_lista = [1, 2, 3, 4]
        expect(list([2, 3, 4, 1]).call(una_lista)).to be false
        expect(list([2, 3, 4, 1], false).call(una_lista)).to be false
      end

      it 'da true si los elementos coinciden, el tamaño no, y matches_size? = false' do
        una_lista = [1, 2, 3, 4]
        expect(list([1, 2, 3], false).call(una_lista)).to be true
      end

      it 'da false si los elementos coinciden, el tamaño no, y matches_size? = true' do
        una_lista = [1, 2, 3, 4]
        expect(list([1, 2, 3]).call(una_lista)).to be false
      end
    end

    describe 'matcher de pato' do
      it 'da true si el objeto entiende el mensaje (uno)' do
        expect(duck(:length).call("Testing")).to be true
      end

      it 'da true si el objeto entiende los mensajes (varios)' do
        expect(duck(:length, :to_sym).call("Testing")).to be true
      end

      it 'da false si el objeto no entiende el mensaje (uno)' do
        expect(duck(:length).call(5)).to be false
      end

      it 'da false si el objeto no entiende al menos uno de los mensajes (varios)' do
        expect(duck(:-, :length, :+).call("Testing")).to be false
      end
    end

    describe 'combinator and' do
      it 'da true si todos los matchers se cumplen' do
        expect(duck(:length).and(type(String)).call("Testing")).to be true
        expect(type(Integer).and(val(5)).call(5)).to be true
        expect(type(Integer).and(val(5), duck(:+)).call(5)).to be true
      end

      it 'da false si al menos una de los matchers no se cumple' do
        expect(duck(:length).and(type(Integer)).call("Testing")).to be false
        expect(type(String).and(val(5)).call(5)).to be false
        expect(type(Integer).and(val(4), duck(:+)).call(5)).to be false
      end
    end

    describe 'combinator or' do
      it 'da false si todos los matchers no se cumplen' do
        expect(duck(:length).or(type(String)).call(5)).to be false
        expect(type(Integer).or(val(5)).call("Testing")).to be false
        expect(type(Integer).or(val(5), duck(:-)).call("Testing")).to be false
      end

      it 'da true si al menos una de los matchers se cumple' do
        expect(duck(:length).and(type(Integer)).call("Testing")).to be true
        expect(type(String).and(val(5)).call(5)).to be true
        expect(type(Integer).and(val(4), duck(:+)).call(5)).to be true
      end
    end

    describe 'combinator not' do
      it 'da true en false' do
        expect(duck(:-).not.call("Testing")).to be true
        expect(val(4).not.call(5)).to be true
      end

      it 'da false en true' do
        expect(duck(:length).not.call("Testing")).to be false
        expect(val(5).not.call(5)).to be false
      end
    end

    describe 'matches?' do
      it 'ejecuta el bloque al encontrar el patron' do
        resultado = false
        matches?(5) do
          with(val(5)) {resultado = true}
        end
        expect(resultado).to be true
      end

      it 'deja de buscar al encontrar un patron correcto' do
        resultado = 0
        matches?(5) do
          with(type(String)) { resultado = 1 }
          with(val(5)) {resultado = 2}
          with(duck(:+)) {resultado = 3}
        end
        expect(resultado).to eq(2)
      end

      it 'ejecuta el bloque de otherwise si no encuentra patron' do
        resultado1 = 0
        matches?(5) do
          otherwise {resultado1 = 1}
          resultado1 = 2
        end

        resultado2 = 0
        matches?(5) do
          with(type(String)) {resultado2 = 1}
          otherwise {resultado2 = 2}
          resultado2 = 3
        end

        expect(resultado1).to eq(1)
        expect(resultado2).to eq(2)
      end
    end
  end
end
