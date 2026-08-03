#lang racket
(require "nfa.rkt" "dfa.rkt")

(provide (struct-out automaton-graph)
         nfa->graph
         dfa->graph)

(struct automaton-graph (states start accept edges) #:transparent)

(define (label-key l) (if (eq? l 'epsilon) 1 0))

(define (edge<? a b)
  (define fa (car a)) (define fb (car b))
  (define la (cadr a)) (define lb (cadr b))
  (define ta (caddr a)) (define tb (caddr b))
  (cond
    [(< fa fb) #t]
    [(> fa fb) #f]
    [(< (label-key la) (label-key lb)) #t]
    [(> (label-key la) (label-key lb)) #f]
    [(and (eq? la 'epsilon) (eq? lb 'epsilon)) (< ta tb)]
    [(char=? la lb) (< ta tb)]
    [else (char<? la lb)]))

(define (sorted-edges raw)
  (sort raw edge<?))

(define (nfa->graph nfa)
  (automaton-graph (nfa-states nfa)
                   (nfa-start nfa)
                   (list (nfa-accept nfa))
                   (sorted-edges
                    (for*/list ([(s lst) (in-hash (nfa-edges nfa))]
                                [e (in-list lst)])
                      (list s (car e) (cdr e))))))

(define (dfa->graph dfa)
  (automaton-graph (dfa-states dfa)
                   (dfa-start dfa)
                   (dfa-accept dfa)
                   (sorted-edges
                    (for*/list ([(s row) (in-hash (dfa-trans dfa))]
                                [(c t) (in-hash row)])
                      (list s c t)))))
