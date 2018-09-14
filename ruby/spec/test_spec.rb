describe 'PatternMatching' do

  describe 'Test unitarios de Pattern Matching' do

    let (:pat_mtc) { PatternMatching.new nil }

    describe 'matcher de variable' do

      it 'devuelve siempre true' do
        expect(:a.call(2)).to be true
        expect(:a.call("Testing")).to be true
      end
    end

    describe 'matcher de valor' do
      it 'devuelve true si coinciden los valores' do
        expect(pat_mtc.val(5).call(5)).to be true
        expect(pat_mtc.val('5').call('5')).to be true
      end

      it 'devuelve false si no coinciden los valores' do
        expect(pat_mtc.val(5).call(4)).to be false
        expect(pat_mtc.val(5).call('5')).to be false
        expect(pat_mtc.val('5').call('4')).to be false
      end
    end

    describe 'matcher de tipo' do
      it 'devuelve true si es del tipo correcto' do
        expect(pat_mtc.type(Integer).call(5)).to be true
        expect(pat_mtc.type(Symbol).call(:a)).to be true
      end

      it 'devuelve false si no es del tipo correcto' do
        expect(pat_mtc.type(Integer).call('5')).to be false
        expect(pat_mtc.type(Symbol).call("a")).to be false
      end
    end

    describe 'matcher de lista' do

      let(:una_lista) { [1, 2, 3, 4] }

      it 'da true si lista es la misma' do
        expect(pat_mtc.list(una_lista).call(una_lista)).to be true
        expect(pat_mtc.list(una_lista, false).call(una_lista)).to be true
      end

      it 'da false si coiciden los tamaños pero no los elementos' do
        expect(pat_mtc.list([2, 3, 4, 1]).call(una_lista)).to be false
        expect(pat_mtc.list([2, 3, 4, 1], false).call(una_lista)).to be false
      end

      it 'da true si los elementos coinciden, el tamaño no, y matches_size? = false' do
        expect(pat_mtc.list([1, 2, 3], false).call(una_lista)).to be true
      end

      it 'da false si los elementos coinciden, el tamaño no, y matches_size? = true' do
        expect(pat_mtc.list([1, 2, 3]).call(una_lista)).to be false
      end

      it 'da true si los patrones coinciden' do
        expect(pat_mtc.list([pat_mtc.val(1), pat_mtc.duck(:+), pat_mtc.type(Integer), pat_mtc.val(4)]).call(una_lista)).to be true
      end

      it 'da false si alguno de los patrones no coincide' do
        expect(pat_mtc.list([pat_mtc.val(1), pat_mtc.duck(:length), pat_mtc.type(Integer), pat_mtc.val(4)]).call(una_lista)).to be false
      end

      it 'da true mezclando patrones y valores' do
        expect(pat_mtc.list([pat_mtc.val(1), 2, pat_mtc.type(Integer), 4]).call(una_lista)).to be true
      end
    end

    describe 'matcher de pato' do
      it 'da true si el objeto entiende el mensaje (uno)' do
        expect(pat_mtc.duck(:length).call("Testing")).to be true
      end

      it 'da true si el objeto entiende los mensajes (varios)' do
        expect(pat_mtc.duck(:length, :to_sym).call("Testing")).to be true
      end

      it 'da false si el objeto no entiende el mensaje (uno)' do
        expect(pat_mtc.duck(:length).call(5)).to be false
      end

      it 'da false si el objeto no entiende al menos uno de los mensajes (varios)' do
        expect(pat_mtc.duck(:-, :length, :+).call("Testing")).to be false
      end
    end

    describe 'combinator and' do
      it 'da true si todos los matchers se cumplen' do
        expect(pat_mtc.duck(:length).and(pat_mtc.type(String)).call("Testing")).to be true
        expect(pat_mtc.type(Integer).and(pat_mtc.val(5)).call(5)).to be true
        expect(pat_mtc.type(Integer).and(pat_mtc.val(5), pat_mtc.duck(:+)).call(5)).to be true
      end

      it 'da false si al menos una de los matchers no se cumple' do
        expect(pat_mtc.duck(:length).and(pat_mtc.type(Integer)).call("Testing")).to be false
        expect(pat_mtc.type(String).and(pat_mtc.val(5)).call(5)).to be false
        expect(pat_mtc.type(Integer).and(pat_mtc.val(4), pat_mtc.duck(:+)).call(5)).to be false
      end
    end

    describe 'combinator or' do
      it 'da false si todos los matchers no se cumplen' do
        expect(pat_mtc.duck(:length).or(pat_mtc.type(String)).call(5)).to be false
        expect(pat_mtc.type(Integer).or(pat_mtc.val(5)).call("Testing")).to be false
        expect(pat_mtc.type(Integer).or(pat_mtc.val(5), pat_mtc.duck(:-)).call("Testing")).to be false
      end

      it 'da true si al menos una de los matchers se cumple' do
        expect(pat_mtc.duck(:length).or(pat_mtc.type(Integer)).call("Testing")).to be true
        expect(pat_mtc.type(String).or(pat_mtc.val(5)).call(5)).to be true
        expect(pat_mtc.type(Integer).or(pat_mtc.val(4), pat_mtc.duck(:+)).call(5)).to be true
      end
    end

    describe 'combinator not' do
      it 'da true en false' do
        expect(pat_mtc.duck(:-).not.call("Testing")).to be true
        expect(pat_mtc.val(4).not.call(5)).to be true
      end

      it 'da false en true' do
        expect(pat_mtc.duck(:length).not.call("Testing")).to be false
        expect(pat_mtc.val(5).not.call(5)).to be false
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

      it 'levanta PatterNotFound si termina los with sin otherwise' do
        expect do
          matches?(false) do
            with(val(true)) {}
          end
        end.to raise_error(PatternNotFound)
      end
    end

    describe 'bindings' do
      it 'simple' do
        resultado = false
        matches?(true) do
          with(:a) { resultado = a }
        end

        expect(resultado).to be true
      end

      it 'simple + patron' do
        resultado = false
        matches?(true) do
          with(:a, val(true)) { resultado = a }
        end

        expect(resultado).to be true
      end

      it 'simple y condicion' do
        resultado = false
        matches?(true) do
          with(:a.and(val(true))) { resultado = a }
        end

        expect(resultado).to be true
      end

      it 'condicion y simple' do
        resultado = false
        matches?(true) do
          with(val(true).and(:a)) { resultado = a }
        end

        expect(resultado).to be true
      end

      it 'lista' do
        resultado = false
        matches?([true, false, true, false]) do
          with(list([:a, :b, :c, :d])) {resultado = a}
        end

        expect(resultado).to be true
      end

      it 'lista parcial' do
        resultado = false
        matches?([1, true, 3]) do
          with(list([1, :a, 3])) {resultado = a}
        end

        expect(resultado).to be true
      end

      it 'lista y simple' do
        resultado = 0
        matches?([1, 1]) do
          with(list([:a, :b]).and(:arr)) { resultado = a + b + arr.first + arr.last }
        end

        expect(resultado).to eq 4
      end

      it 'simple y lista' do
        resultado = 0
        matches?([1, 1]) do
          with(:arr.and(list([:a, :b]))) { resultado = a + b + arr.first + arr.last }
        end

        expect(resultado).to eq 4
      end

      it 'simple + lista' do
        resultado = 0
        matches?([1, 1]) do
          with(:arr, list([:a, :b])) { resultado = a + b + arr.first + arr.last }
        end

        expect(resultado).to eq 4
      end
    end
  end
end
