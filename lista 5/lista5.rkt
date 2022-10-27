;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lista5) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; ==========================================================
;; DEFINIÇÃO DE DADOS E FUNÇÕES PARA UTILIZAREM NESTA LISTA
;; ==========================================================

(define-struct carta (naipe valor))
;; Um elemento do conjunto Carta é
;;   (make-carta n v)     onde
;;   n : String, é o naipe da carta, que pode ser "copas", "ouros", "espadas" ou "paus"
;;   v : Número, é o valor da carta, que pode ser qualquer inteiro entre 1 e 10

(define CARTA-NULA (make-carta "nenhum" 0))


;; soma15?: Carta Carta -> Booleano
;; obj: dadas 2 cartas, verifica se o valor delas somado é 15.
;; ex:
;;  (soma15? $CARTA3O $CARTA5C) -> #false
;;  (soma15? $CARTA10O $CARTA5C) -> #true)
;;  (soma15? $CARTA5C $CARTA10O) -> #true)
;;  (soma15? $CARTA7E $CARTA8E) -> #true)

(define (soma15? c1 c2)
  (= 15 (+ (carta-valor c1) (carta-valor c2))))

(check-expect (soma15? (make-carta "ouros" 3) (make-carta "copas" 5)) #false)
(check-expect (soma15? (make-carta "ouros" 10) (make-carta "copas" 5)) #true)
(check-expect (soma15? (make-carta "copas" 5) (make-carta "ouros" 10)) #true)
(check-expect (soma15? (make-carta "espadas" 7) (make-carta "espadas" 8)) #true)



;; ==========================================================
;; QUESTÃO 1
;; ==========================================================

;; --------------------
;; TIPO MÃO:
;; --------------------

;; Uma Mão é:
;; 1. uma lista vazia empty, ou
;; 2. (cons elemento resto), onde:
;;    elemento: um elemento do conjunto Carta e
;;    resto: Mão.

;; Exemplos de constantes do tipo Mão:

(define MÃO_1 (cons (make-carta "ouros" 5)
                    (cons (make-carta "paus" 5)
                          (cons (make-carta "copas" 1) empty))))

(define MÃO_2 (cons (make-carta "espadas" 2)
                    (cons (make-carta "paus" 5)
                          (cons (make-carta "copas" 6) empty))))

(define MÃO_3 (cons (make-carta "espadas" 3)
                    (cons (make-carta "ouros" 2)
                          (cons (make-carta "copas" 4) empty))))

(define MÃO_4 (cons (make-carta "copas" 7)
                    (cons (make-carta "ouros" 3)
                          (cons (make-carta "copas" 4) empty))))

(define MÃO_5 (cons (make-carta "copas" 2)
                    (cons (make-carta "ouros" 3)
                          (cons (make-carta "copas" 4) empty))))

;; --------------------
;; TIPO MESA:
;; --------------------

;; Uma Mesa é:
;; 1. uma lista vazia empty, ou
;; 2. (cons elemento resto), onde:
;;    elemento: um elemento do conjunto Carta e
;;    resto: Mesa.

;; Exemplos de constantes do tipo Mesa:

(define MESA_1 (cons (make-carta "ouros" 2)
                     (cons (make-carta "paus" 2)
                           (cons (make-carta "copas" 5)
                                 (cons (make-carta "espadas" 2) empty)))))

(define MESA_2 (cons (make-carta "ouros" 1)
                     (cons (make-carta "paus" 2)
                           (cons (make-carta "copas" 1)
                                 (cons (make-carta "espadas" 8) empty)))))

(define MESA_3 (cons (make-carta "copas" 3)
                     (cons (make-carta "ouros" 1)
                           (cons (make-carta "copas" 1)
                                 (cons (make-carta "espadas" 3) empty)))))

(define MESA_4 (cons (make-carta "copas" 8)
                     (cons (make-carta "ouros" 1)
                           (cons (make-carta "copas" 1)
                                 (cons (make-carta "espadas" 3) empty)))))

(define MESA_5 (cons (make-carta "ouros" 2)
                     (cons (make-carta "paus" 2)
                           (cons (make-carta "espadas" 2)
                                 (cons (make-carta "copas" 5) empty)))))


;; ===========================================================
;; QUESTÃO 2
;; ===========================================================
;; soma15: Carta Mesa -> Carta
;; Obj: dada uma carta e uma mesa, retorna a carta da mesa que
;; soma 15 com o carta dada.
;; Exemplos:
;; (soma15 (make-carta "copas" 10) empty) = CARTA-NULA
;; (soma15 (make-carta "copas" 10) MESA_1) = (make-carta "copas" 5)
;; (soma15 (make-carta "copas" 10) MESA_5) = (make-carta "copas" 5)
;; (soma15 (make-carta "copas" 10) MESA_4) = CARTA-NULA


;; Função principal:

(define (soma15 @uma-carta @uma-mesa)
  (cond
    ;; Se @uma-mesa é uma lista vazia, retorna CARTA-NULA
    [(empty? @uma-mesa) CARTA-NULA]
    ;; Se @uma-mesa não for vazia, retorna a carta apropriada
    [else 
     (cond
       ;; Se a primeira carta de @uma-mesa somar 15 com a
       ;; carta dada, retorna a primeira carta de @uma-mesa
       [(= (+ 
            (carta-valor @uma-carta) 
            (carta-valor (first @uma-mesa))
            ) 15) (first @uma-mesa)]
       ;; Caso contrário, retorna a carta de @uma-mesa que
       ;; soma 15 com a carta dada
       [else (soma15 @uma-carta (rest @uma-mesa))]
       )]
    )
  )

;; Testes:

(check-expect (soma15 (make-carta "espadas" 7) empty) CARTA-NULA)
(check-expect (soma15 (make-carta "espadas" 7) MESA_4) (make-carta "copas" 8)) 
(check-expect (soma15 (make-carta "espadas" 10) MESA_5) (make-carta "copas" 5))
(check-expect (soma15 (make-carta "espadas" 7) MESA_3) CARTA-NULA) 



;; ============================================================
;; QUESTÃO 3
;; ============================================================
;; soma-mesa: Mesa -> Número
;; Obj: dada uma mesa, soma os valores das cartas que estão contidas
;; nela.
;; Exemplos:
;; (soma-mesa empty)  = 0
;; (soma-mesa MESA_2) = 11

;; Função auxiliar:

(define (soma-mesa @uma-mesa)
  (cond
    ;; Se @uma-mesa é uma lista vazia, retorna 0
    [(empty? @uma-mesa) 0]
    ;; Caso contrário, soma os valores das cartas de @uma-mesa
    [else (+ (carta-valor (first @uma-mesa)) 
             (soma-mesa (rest @uma-mesa)))] 
    )
  )

;; Testes:

(check-expect (soma-mesa empty) 0)
(check-expect (soma-mesa MESA_1) 11)

;; escova?: Carta Mesa -> Booleano
;; Obj: dada uma carta e uma mesa, verifica se a carta dada
;; faz escova com as cartas da mesa.
;; Exemplos:
;; (escova? (make-carta "ouros" 7) empty)  = false
;; (escova? (make-carta "ouros" 7) MESA_3) = true
;; (escova? (make-carta "ouros" 8) MESA_3) = false

;; Função principal:

(define (escova? @uma-carta @uma-mesa)
  (cond
    ;; Se @uma-mesa é uma lista vazia, retorna false
    [(empty? @uma-mesa) false]
    ;; Se @uma-mesa não for uma lista vazia, verifica
    ;; se a carta dada faz escova com as cartas da mesa
    [else (cond
            ;; Se a carta dada somar 15 com a soma dos valores
            ;; das cartas da mesa, retorna true
            [(= (+
                 (carta-valor @uma-carta) 
                 (soma-mesa @uma-mesa)
                 )       
                15) true]
            ;; Caso contrário, retorna false
            [else false]
            )] 
    )
  )

;; Testes:

(check-expect (escova? (make-carta "ouros" 4) empty) false)
(check-expect (escova? (make-carta "ouros" 4) MESA_1) true)
(check-expect (escova? (make-carta "ouros" 5) MESA_1) false)

;; =============================================================
;; QUESTÃO 4
;; =============================================================
;; jogada-escova: Mão Mesa -> String
;; Obj: dada uma mão e uma mesa, retorna a string "Não faço escova.",
;; quando uma carta da mão não somar 15 com a soma dos valores das cartas
;; da mesa, ou retorna "Escova!" quando uma carta da mão somar 15 com
;; a soma dos valores das cartas da mesa.
;; Exemplos:
;; (jogada-escova empty MESA_3) = "Não faço escova."
;; (jogada-escova MÃO_2 empty)  = "Não faço escova."
;; (jogada-escova MÃO_4 MESA_3) = "Escova!"
;; (jogada-escova MÃO_5 MESA_5) = "Escova!"
;; (jogada-escova MÃO_5 MESA_3) = "Não faço escova."

;; Função principal:

(define (jogada-escova @uma-mão @uma-mesa)
  (cond
    ;; Se @uma-mão é uma lista vazia, retorna "Não faço escova."
    [(empty? @uma-mão) "Não faço escova."]
    ;; Se @uma-mesa é uma lista vazia, retorna "Não faço escova."
    [(empty? @uma-mesa) "Não faço escova."]
    ;; Se @uma-mão e @uma-mesa não forem listas vazias, retorna
    ;; "Escova!" quando houver escova.
    [else (cond
            ;; Se a primeira carta de @uma-mão somar 15 com a soma dos
            ;; valores das cartas de @uma-mesa, retorna "Escova!".
            [(= (+
                 (carta-valor (first @uma-mão))
                 (soma-mesa @uma-mesa)
                 )
              15) "Escova!"]
            ;; Se alguma carta de @uma-mão somar 15 com a soma dos valores
            ;; das cartas de @uma-mesa, retorna "Escova!".
	    [else (jogada-escova (rest @uma-mão) @uma-mesa)]
            )] 
    )
  )

;; Testes:

(check-expect (jogada-escova empty MESA_1) "Não faço escova.")
(check-expect (jogada-escova MÃO_1 empty) "Não faço escova.")
(check-expect (jogada-escova MÃO_5 MESA_4) "Escova!")
(check-expect (jogada-escova MÃO_3 MESA_4) "Escova!")
(check-expect (jogada-escova MÃO_1 MESA_4) "Não faço escova.")

;; ==============================================================
;; QUESTÃO 5
;; ==============================================================

(define-struct jogada (carta msg))
;; Um elemento do conjunto Jogada é
;;   (make-jogada c m)     onde
;;   c : Carta, é a carta selecionada na jogada
;;   m : String, é uma mensagem, que pode ser "Escova!" ou "Não faço escova."

;; seleciona-jogada: Mão Mesa -> Carta
;; Obj: dada uma mão e uma mesa, retorna a carta da mão a ser jogada.
;; Exemplos:
;; (seleciona-jogada MÃO_3 MESA_5) = (make-carta "ouros" 2)
;; (seleciona-jogada MÃO_3 MESA_2) = (make-carta "espadas" 3)

(define (seleciona-jogada @uma-mão @uma-mesa)
  (cond
    ;; Se o valor primeira carta de @uma-mão somar 15 com a soma
    ;; dos valores das cartas de @uma-mesa, retorna a primeira carta
    ;; de @uma-mão.
    [(= (+
         (carta-valor (first @uma-mão))
         (soma-mesa @uma-mesa)
         ) 
        15) (first @uma-mão)]
    ;; Caso contrário, retorna uma carta subsquente de @uma-mão que faz
    ;; escova com @uma-mesa.
    [else (seleciona-jogada (rest @uma-mão) @uma-mesa)]
    
    )
  )

;; Testes:

(check-expect (seleciona-jogada MÃO_3 MESA_4) (make-carta "ouros" 2))
(check-expect (seleciona-jogada MÃO_2 MESA_4) (make-carta "espadas" 2))

;; seleciona-carta: Mão Mesa -> Jogada
;; Obj: dada uma mão e uma mesa, retorna o conjunto Jogada, que é composto
;; por uma carta e uma mensagem.
;; Exemplos:
;; (seleciona-carta MÃO_3 MESA_5) = (make-jogada (make-carta "copas" 4) "Escova!")
;; (seleciona-carta MÃO_4 MESA_4) = (make-jogada (make-carta "copas" 7) "Não faço escova.")

;; Função principal:

(define (seleciona-carta @uma-mão @uma-mesa)
  (make-jogada (cond
                 ;; Se a string da função jogada-escova for igual a "Escova!", retorna
                 ;; o resultado da função auxiliar seleciona-jogada, que, por sua vez,
                 ;; retorna a carta de @uma-mão que faz escova com as cartas de @uma-mesa.
                 [(string=? (jogada-escova @uma-mão @uma-mesa) "Escova!")
                  (seleciona-jogada @uma-mão @uma-mesa)]
                 [else (first @uma-mão)]
                 )
               ;; Retorna a string da função jogada-escova.
               (jogada-escova @uma-mão @uma-mesa)
               )
  )

;; Testes:

(check-expect (seleciona-carta MÃO_3 MESA_4) (make-jogada (make-carta "ouros" 2) "Escova!"))
(check-expect (seleciona-carta MÃO_1 MESA_4) (make-jogada (make-carta "ouros" 5) "Não faço escova.")) 

;; ==============================================================
;; DEFINIÇÃO DE FUNÇÕES PARA DESENHAR UMA CARTA
;; ==============================================================

;; rectangle-round: Número Número String Número -> Imagem
;; obj: dado uma cor, uma altura e largura e uma escala, retorna um retângulo de cantos arredondados.
(define (rectangle-round largura altura cor escala)
  (scale escala
         (above (beside (crop 0 0 5 5 (circle 5 "solid" cor)) (rectangle (- largura 10) 5 "solid" cor) (crop 5 0 5 5 (circle 5 "solid" cor)))
                (rectangle largura (- altura 10) "solid" cor)
                (beside (crop 0 5 5 5 (circle 5 "solid" cor)) (rectangle (- largura 10) 5 "solid" cor) (crop 5 5 5 5 (circle 5 "solid" cor))))))

;; carta-frame: String -> Imagem
;; obj: dado uma cor, retorna o formato base de uma carta.
(define (carta-frame cor)
  (overlay (rectangle 65 90 "outline" cor) (rectangle-round 110 160 "white" 1) (rectangle-round 125 175 cor 1) (rectangle-round 130 180 "white" 1)))

;; coracao: String -> Imagem
;; obj: dado uma cor, retorna a imagem de um coração.
(define (coracao cor)
  (overlay/align/offset "center" "top" (overlay/offset (circle 50 "solid" cor) 80 0 (circle 50 "solid" cor)) 0 75 (rotate 180 (isosceles-triangle 130 80 "solid" cor))))

;; circulos: String -> Imagem
;; obj: dado uma cor, retorna a imagem de três circulos, como no naipe de paus do baralho.
(define (circulos cor) (overlay/align/offset "center" "top" (overlay/offset (circle 50 "solid" cor) 80 0 (circle 50 "solid" cor)) 0 75 (circle 50 "solid" cor)))

;; logo-ouros: Número String -> Imagem
;; obj: dado uma escala e uma cor, retorna o logo de ouros.
(define (logo-ouros escala cor) (scale escala (rhombus 120 60 "solid" cor)))

;; logo-copas Número String -> Imagem
;; obj: dado uma escala e uma cor, retorna o logo de copas.
(define (logo-copas escala cor) (scale escala (coracao cor)))

;; logo-espadas Número String -> Imagem
;; obj: dado uma escala e uma cor, retorna o logo de espadas.
(define (logo-espadas escala cor) (scale escala (overlay/align/offset "center" "bottom" (rotate 180 (coracao cor)) 0 30 (isosceles-triangle 80 80 "solid" cor))))

;; logo-paus: Número String -> Imagem
;; obj: dado uma escala e uma cor, retorna o logo de paus.
(define (logo-paus escala cor) (scale escala (overlay/align/offset "center" "bottom" (rotate 180 (circulos cor)) 0 30 (isosceles-triangle 80 80 "solid" cor))))

(define (desenha-logo n escala cor)
  (cond [(string=? n "ouros") (logo-ouros escala cor)]
        [(string=? n "espadas") (logo-espadas escala cor)]
        [(string=? n "paus") (logo-paus escala cor)]
        [(string=? n "copas") (logo-copas escala cor)]
        [else empty-image]))

;; desenha-carta: Carta -> Imagem
;; obj: dada uma carta, gera uma visualização desta carta. Se a carta for nula, gerar a imagem vazia.
(define (desenha-carta c)
  (desenha-carta-escala c 0.5))

;; desenha-carta-escala: Carta Número -> Imagem
;; obj: dada uma carta e uma escala, gera uma visualização desta carta. Se a carta for nula, gerar a imagem vazia.
(define (desenha-carta-escala c escala)
  (cond [(string=? "paus" (carta-naipe c)) (desenha-carta-cor c "Cornflower Blue" escala)]
        [(string=? "espadas" (carta-naipe c)) (desenha-carta-cor c "Cornflower Blue" escala)]
        [(string=? "ouros" (carta-naipe c)) (desenha-carta-cor c "Crimson" escala)]
        [(string=? "copas" (carta-naipe c)) (desenha-carta-cor c "Crimson" escala)]
        [else empty-image]))

;; desenha-carta-escala: Carta Número -> Imagem
;; obj: dada uma carta, uma cor e uma escala, gera uma visualização desta carta. Se a carta for nula, gerar a imagem vazia.
(define (desenha-carta-cor carta cor escala)
  (scale escala (overlay/align "middle" "center"
                               (desenha-logo (carta-naipe carta) 0.2 cor)
                               (overlay/align/offset "right" "top"
                                                     (text (number->string (carta-valor carta)) 25 cor)
                                                     20 -20
                                                     (overlay/align/offset "left" "bottom"
                                                                           (rotate 180 (text (number->string (carta-valor carta)) 25 cor))
                                                                           -20 20
                                                                           (carta-frame cor))))))

;; ==============================================================
;; QUESTÃO EXTRA
;; ==============================================================
