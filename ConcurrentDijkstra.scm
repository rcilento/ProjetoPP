#lang racket
;Grafo que será analisado
(define G1 '(
             (a . ((b . 7) (f . 9) (c . 14)))
             (b . ((a . 7) (d . 15) (f . 10)))
             (c . ((a . 14) (f . 2) (e . 9)))
             (d . ((b . 15) (f . 11) (e . 6)))
             (e . ((c . 9) (d . 6)))  
             (f . ((c . 2) (d . 11) (b . 10) (a . 9)))))
;Lista os vertices do grafo
(define (vertices graph)
  (map (lambda (n) (car n)) graph))
;Deleta um atomo de uma lista
(define (delete item list)
  (cond
    ((null? list) list)
    ((equal? item (car list)) (cdr list))
    (else (cons (car list) (delete item (cdr list))))))
;Devolve a aridade de uma lista
(define (count-list list num)
  (cond
    ((null? list)num)
    (else (count-list (cdr list) (+ 1 num)))))
;;Define um resp com distancia infinita até todos os vertices
(define (passo1 list2 list1)
  (cond
    ((null? list1)(reverse list2))
    (else (passo1 (cons (cons (car list1) +inf.0)list2)(cdr list1)))))
;Define como distancia zero o caminho até a origem
(define (set-origem temp resp origem)
  (cond
    ((null? resp) (reverse temp))
     ((equal? (car(car resp)) origem) (set-origem (cons (cons origem 0)temp) (cdr resp) origem))
     (else (set-origem (cons (car resp) temp) (cdr resp) origem))))

;;;; As funções abaixo deverão ser executadas pelas threads produtoras

;Devolve o vetor dos vizinhos de um vertice (dados retirados dos grafo)
(define (vert-vizinhos vert graph)
  (cond
    ((equal? vert (car(car graph)))(cdr(car graph)))
     (else (vert-vizinhos vert (cdr graph)))))                              

;Busca no resp (vetor com os menores caminhos até um vertice) o peso do vertice que está sendo analisado   
(define (achar-peso vert resp)
	(cond ((equal? vert (car(car resp))))
	(else (achar-peso vert (cdr resp)))))

;Processa a lista de vizinhos junto com o peso do vertice e devolve uma lista da distancia real obtida na analise	
(define (soma-peso peso LIST vizinhos)
	(cond 
	((null? vizinhos) LIST)
	(else (soma-peso peso (cons(cons (car(car vizinhos)) (+ peso (cdr(car vizinhos)))) LIST) (cdr vizinhos)))))
	

;;;; Funçao abaixo deverá ser executado pela thread consumidora principal

;Analisa um LIST de um vertice analisado e compara com as distancias do resp, escolhendo as distancias minimas
(define (main temp resp LIST)
		  (cond
		  	((null? LIST) resp)
		  	((null? resp) (main '() temp (cdr LIST)))
		  	((equal? (car(car LIST)) (car (car resp))) (main (cons (cons (car(car LIST)) (min (cdr(car resp))(cdr(car LIST))))temp) (cdr resp) LIST))
		  	(else (main (cons (car resp)temp) (cdr resp) LIST))))    
  
(define unexplored (vertices G1)) 

;Teste se tudo está funcionando por hora
(display (main '()(set-origem '() (passo1 '() unexplored) 'f) (soma-peso (achar-peso 'f (set-origem '() (passo1 '() unexplored) 'f)) '() (vert-vizinhos 'f G1))))
