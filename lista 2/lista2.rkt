;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lista2) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; ==========================================================
;; QUESTÃO 1
;; ==========================================================

;; movimenta-personagem: Símbolo -> String
;; Obj: a função retorna uma string (mensagem), a qual representa a ação
;; realizada por um símbolo.
;; Exemplos:
;; (movimenta-personagem 'w) = "Andando para frente"
;; (movimenta-personagem 'a) = "Andando para esquerda"
;; (movimenta-personagem 's) = "Andando para tras"
;; (movimenta-personagem 'd) = "Andando para direita"
;; (movimenta-personagem 'm) = "Parado"
;; (movimenta-personagem 'n) = "Parado"
;; (movimenta-personagem 'o) = "Parado"

;; Função principal:

(define (movimenta-personagem @tecla)
  (cond
    ;; se o símbolo escolhido for 'w, retorna "Andando para frente".
    [(symbol=? @tecla 'w) "Andando para frente"]
    ;; se o símbolo escolhido for 'a, retorna "Andando para esquerda".
    [(symbol=? @tecla 'a) "Andando para esquerda"]
    ;; se o símbolo escolhido for 's, retorna "Andando para tras".
    [(symbol=? @tecla 's) "Andando para tras"]
    ;; se o símbolo escolhido for 'd, retorna "Andando para direita".
    [(symbol=? @tecla 'd) "Andando para direita"]
    ;; caso contrário, retorna "Parado".
    [else "Parado"]))

;; Testes:
(check-expect (movimenta-personagem 'w) "Andando para frente")
(check-expect (movimenta-personagem 'a) "Andando para esquerda")
(check-expect (movimenta-personagem 's) "Andando para tras")
(check-expect (movimenta-personagem 'd) "Andando para direita")
(check-expect (movimenta-personagem 't) "Parado")
(check-expect (movimenta-personagem 'u) "Parado")
(check-expect (movimenta-personagem 'v) "Parado")

;; ===========================================================
;; QUESTÃO 2
;; ===========================================================
;; distância-entre-pontos : Número Número Número Número Número -> Número
;; Obj: Retorna a distância entre os pontos.
;; Exemplos:
;; (distância-entre-pontos 0 0 2 1 1)   = #i1.4142135623730951
;; (distância-entre-pontos -1 -1 5 2 2) = #i4.242640687119285
;; (distância-entre-pontos 1 9 10 5 5)  = #i5.656854249492381

;; Função auxiliar:

(define (distância-entre-pontos @xc @yc @r @xp @yp)
  (sqrt
   (+
    (expt (- @xc @xp) 2)
    (expt (- @yc @yp) 2))))

;; Testes:

(check-within (distância-entre-pontos 5 5 2 4 1) 4.12 0.03)
(check-within (distância-entre-pontos 0 0 1 1 1) 1.41 0.02)
(check-within (distância-entre-pontos -2 -3 4 2 2) 6.40 0.01)

;; esta-dentro? : Número Número Número Número Número -> Booleano
;; Obj: Verificar se dois pontos estão dentro da circuferência
;; e se estão sobre a circunferência.
;; Exemplos:
;;  (esta-dentro? 0 0 5 2 3)     = #true
;;  (esta-dentro? -1 -4 5 -2 -2) = #true
;;  (esta-dentro? 0 0 3 7 10)    = #false
;;  (esta-dentro? -1 -4 5 1 -10) = #false

;; Função principal:

(define (esta-dentro? @xc @yc @r @xp @yp)
  (cond
    ;; se o resultado da fómula da distância entre pontos for menor ou igual ao raio, retorna #true.
    [(<=(distância-entre-pontos @xc @yc @r @xp @yp) @r) true]
    ;; caso contrário, retorna #false.
    [else false]))

;; Testes:

(check-expect (esta-dentro? -3 1 5 -1 4) true)
(check-expect (esta-dentro? 0 0 2 1 1) true)
(check-expect (esta-dentro? 1 1 5 1 7) false)
(check-expect (esta-dentro? -1 -1 2 3 5) false)

;; ============================================================
;; QUESTÃO 3
;; ============================================================
;; esta-dentro?-v2 : Número Número Número Número Número -> Booleano
;; Obj: Verifica se as coordenadas xp e yp estão dentro da circunferência
;; e se estão sobre a circunferência.
;; Exemplos:
;; (esta-dentro?-v2 1 1 5 0 5) = true
;; (esta-dentro?-v2 0 0 8 0 5) = true
;; (esta-dentro?-v2 1 1 4 9 1) = false
;; (esta-dentro?-v2 1 1 4 8 1) = false

;; Função principal:

(define (esta-dentro?-v2 @xc @yc @r @xp @yp)
  (<=(distância-entre-pontos @xc @yc @r @xp @yp) @r))

;; Testes:

(check-expect (esta-dentro?-v2 0 0 5 1 1) true)
(check-expect (esta-dentro?-v2 0 0 5 2 1) true)
(check-expect (esta-dentro?-v2 0 0 5 9 -7) false)
(check-expect (esta-dentro?-v2 0 0 5 -15 -7) false)

;; =============================================================
;; QUESTÃO 4
;; =============================================================
;; gera-resposta : Número -> String
;; Obj: Retorna uma mensagem a partir de um número fornecido
;; (entre 0 e 3).
;; Exemplos:
;; (gera-resposta 3) = 3
;; (gera-resposta 2) = 2
;; (gera-resposta 1) = 1
;; (gera-resposta 0) = 0

;; Função auxiliar:

(define (gera-resposta @número)
  (cond
    [(= @número 3) "Neste mês, foi possível pagar 3 conta(s)."]
    [(= @número 2) "Neste mês, foi possível pagar 2 conta(s)."]
    [(= @número 1) "Neste mês, foi possível pagar 1 conta(s)."]
    [(= @número 0) "Neste mês, foi possível pagar 0 conta(s)."]))

;; Testes:

(check-expect (gera-resposta 3) "Neste mês, foi possível pagar 3 conta(s).")
(check-expect (gera-resposta 2) "Neste mês, foi possível pagar 2 conta(s).")
(check-expect (gera-resposta 1) "Neste mês, foi possível pagar 1 conta(s).")
(check-expect (gera-resposta 0) "Neste mês, foi possível pagar 0 conta(s).")

;; gastos-mes : Número Número Número Número -> String
;; Obj: Retorna uma string (quantidade de contas que podem ser pagas)
;; a partir do salário e das contas de luz e internet e do aluguel.
;; Exemplos:
;; (gastos-mes 4000 1000 1000 1000)  = (gera-resposta 3)
;; (gastos-mes 4000 1000 1000 5000)  = (gera-resposta 2)
;; (gastos-mes 4000 1000 5000 1000)  = (gera-resposta 2)
;; (gastos-mes 4000 5000 1000 1000)  = (gera-resposta 2)
;; (gastos-mes 4000 1000 5000 5000)  = (gera-resposta 1)
;; (gastos-mes 4000 5000 1000 5000)  = (gera-resposta 1)
;; (gastos-mes 4000 5000 5000 1000)  = (gera-resposta 1)
;; (gastos-mes 4000 5000 5000 5000)  = (gera-resposta 0)

;; Função principal:

(define (gastos-mes @salário @aluguel @luz @internet)
  (cond
    ;; se o salário suplantar a soma das três contas, retorna (gera-respostas 3).
    [(>= @salário (+ @aluguel @luz @internet)) (gera-resposta 3)]
    [(or
      (or
       ;; se o salário suplantar a soma da conta de luz com o aluguel, mas se não suplantar a conta de internet,
       ;; retorna (gera-respostas 2).
       (and (>= @salário (+ @aluguel @luz)) (> @internet (- @salário (+ @aluguel @luz))))
       ;; se o salário suplantar a conta da internet e do aluguel, mas se não suplantar a conta de luz,
       ;; retorna (gera-respostas 2).
       (and (>= @salário (+ @aluguel @internet)) (> @luz (- @salário (+ @aluguel @internet)))))
      ;; se o salário suplantar a soma das contas de luz e internet,
      ;; mas se não suplantar o aluguel, retorna (gera-respostas 2).
      (and (>= @salário (+ @luz @internet)) (> @aluguel (- @salário (+ @luz @internet))))) (gera-resposta 2)]
    [(or
     (or
       ;; se o salário suplantar o aluguel, mas se não suplantar a soma das contas de internet e luz,
       ;; retorna (gera-respostas 1).
      (and (>= @salário @aluguel) (< (- @salário @aluguel) (+ @internet @luz)))
       ;; se o salário suplanatar a conta de luz, mas se não suplantar a soma da conta de internet com o aluguel,
       ;; retorna (gera-respostas 1).
       (and (>= @salário @luz) (< (- @salário @luz) (+ @aluguel @internet))))
      ;; se o salário suplantar a conta de internet, mas se não suplantar a soma da conta de luz com o aluguel,
      ;; retorna (gera-respostas 1).
     (and (>= @salário @internet) (< (- @salário @internet)(+ @aluguel @luz)))) (gera-resposta 1)]
    ;; caso contrário, retorna (gera-respostas 0).
    [else (gera-resposta 0)])) 
      
;; Testes:

(check-expect (gastos-mes 4000 2500 250 200) (gera-resposta 3))
(check-expect (gastos-mes 4000 2000 2000 5000) (gera-resposta 2))
(check-expect (gastos-mes 4000 2000 5000 2000) (gera-resposta 2))
(check-expect (gastos-mes 4000 5000 2000 2000) (gera-resposta 2))
(check-expect (gastos-mes 4000 3000 5000 5000) (gera-resposta 1))
(check-expect (gastos-mes 4000 5000 3000 5000) (gera-resposta 1))
(check-expect (gastos-mes 4000 5000 5000 3000) (gera-resposta 1))
(check-expect (gastos-mes 4000 5000 5000 5000) (gera-resposta 0))

;; ==============================================================
;; QUESTÃO 5
;; ==============================================================

;; max-tres : Número Número Número -> Número
;; Obj: Entre três números, retorna o maior.
;; Exemplos:
;; (max-tres 3 1 1)     =  3
;; (max-tres 4 2 1)     =  4
;; (max-tres 4 2 3)     =  4
;; (max-tres 2 7 2)     =  7
;; (max-tres 4 8 2.5)   =  8
;; (max-tres 9 5 5)     =  9
;; (max-tres 16 12 8)   =  16
;; (max-tres 16 7 16)   =  16
;; (max-tres 9 18 9)    =  18
;; (max-tres 7.5 10 5)  =  10
;; (max-tres 5 10 7.5)  =  10
;; (max-tres 5 5 10)    =  10
;; (max-tres 13.5 9 18) =  18
;; (max-tres 8 9.6 16)  =  16
;; (max-tres 7 14 9.8)  =  14
;; (max-tres 9 9 9)     =  9

;; Função principal:

(define (max-tres @a @b @c)
  (cond
    ;; se a for maior do que b:
    [(> @a @b) (cond
               ;; se b for igual a c, retorna a.
               [(= @b @c) @a]
               ;; se b for maior do que c, retorna a.
               [(> @b @c) @a]
               ;; se c for maior do que b:
               [(> @c @b) (cond
                          ;; se c for maior do que a, retorna c.
                          [(> @c @a) @c]
                          ;; caso contrário, retorna a.
                          [else @a]
                          )]
               )]
    ;; se b for maior do que a:
    [(> @b @a) (cond
               ;; se a for igual a b, retorna b.
               [(= @a @c) @b]
               ;; se a for maior do que c, retorna b.
               [(> @a @c) @b]
               ;; se c for maior do que a:
               [(> @c @a) (cond
                          ;; se c for maior do que b, retorna c.
                          [(> @c @b) @c]
                          ;; caso contrário, retorna b.
                          [else @b]
                          )]
               )]
    ;; se c for maior do que a:
    [(> @c @a) (cond
               ;; se a for igual a b, retorna c.
               [(= @a @b) @c]
               )]
    ;; caso contrário:
    [else @a]))

;; Testes:

(check-expect (max-tres 2 1 1) 2)
(check-expect (max-tres 2 1 0.5) 2)
(check-expect (max-tres 2 1 1.5) 2)

(check-expect (max-tres 1 2 1) 2)
(check-expect (max-tres 1 2 0.5) 2)
(check-expect (max-tres 4 2 2) 4)

(check-expect (max-tres 4 3 2) 4)
(check-expect (max-tres 4 2 4) 4)
(check-expect (max-tres 1 2 1) 2)

(check-expect (max-tres 1.5 2 1) 2)
(check-expect (max-tres 1 2 1.5) 2)
(check-expect (max-tres 1 1 2) 2)

(check-expect (max-tres 1.5 1 2) 2)
(check-expect (max-tres 1 1.2 2) 2)
(check-expect (max-tres 1 2 1.4) 2)

(check-expect (max-tres 1 1 1) 1)

;; ==============================================================
;; QUESTÃO EXTRA
;; ==============================================================
;; subida: Número Número Número -> Número
;; Obj: Determinar quantas ações devem ser compradas.
;; Exemplos:
;; (subida 7 10 500)  = 71
;; (subida 7 10 1000) = 142

;; Função auxiliar:

(define (subida @preco_abertura @preco_atual @dinheiro_em_caixa)
  (floor(* (/ @dinheiro_em_caixa @preco_atual) (/ @preco_atual @preco_abertura))))

;; Testes:

(check-expect (subida 10 11 5000) 500)
(check-expect (subida 10 10.9 1000) 100)

;; descida: Número Número Número -> Número
;; Obj: Determinar quantas ações devem ser vendidas.
;; Exemplos:
;; (descida 40 36.5 200) = 17
;; (descida 40 33 200)   = 35

;; Função auxiliar:

(define (descida @preco_abertura @preco_atual @qunt_acoes)
  (floor
   (*
    (+(*(/ @preco_atual @preco_abertura) -1)1)
    @qunt_acoes)))

;; Testes:

(check-expect (descida 25 7 1000) 720)
(check-expect (descida 10 5 250) 125)

;; opera-na-bolsa: Número Número Número Número -> Número
;; Obj: Automatiza as operações a serem realizadas, em caso de subida
;; de preço de uma ação, assim como em caso de queda de uma ação.
;; Exemplos:
;; (opera-na-bolsa 10 10.8 1000 5000) = 0
;; (opera-na-bolsa 10 11.5 1000 5000) = 500
;; (opera-na-bolsa 10 30 990 5000)    = -990
;; (opera-na-bolsa 10 9 1000 5000)    = 0
;; (opera-na-bolsa 10 4 1000 5000)    = -600

;; Função principal:

(define (opera-na-bolsa @preco_abertura @preco_atual @qunt_acoes @dinheiro_em_caixa)
  (cond
    [(or
       ;; se a ação subiu até 10%, retorna 0.
      (and
       (>= @preco_atual (* 1.0 @preco_abertura))
       (< @preco_atual (* 1.1 @preco_abertura)))
      ;; se a ação caiu até 20%, retorna 0.
      (and
       (< @preco_atual (* 1.0 @preco_abertura))
       (>= @preco_atual (* 0.8 @preco_abertura)))) 0]
    ;; se a ação subiu entre 10% e 100%, retorna a quantidade de ações a serem compradas.
    [(and
      (>= @preco_atual (* 1.1 @preco_abertura))
      (< @preco_atual (* 2 @preco_abertura))) (subida @preco_abertura @preco_atual @dinheiro_em_caixa)]
    ;; se a ação subiu mais de 100%, vende todas as ações
    [(>= @preco_atual (* 2 @preco_abertura)) (* @qunt_acoes -1)]
    ;; se a ação caiu mais de 20%, retorna a quantidade de ações a serem vendidas.
    [(< @preco_atual (* 0.8 @preco_abertura)) (* (descida @preco_abertura @preco_atual @qunt_acoes) -1)]))

;; Testes:

(check-expect (opera-na-bolsa 10 10.9 1000 5000) 0)
(check-expect (opera-na-bolsa 10 11 1000 5000) 500)
(check-expect (opera-na-bolsa 10 20 1000 5000) -1000)
(check-expect (opera-na-bolsa 10 8.1 1000 5000) 0)
(check-expect (opera-na-bolsa 10 5 250 5000) -125)
