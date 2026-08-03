#lang racket
(require "nfa.rkt" "dfa.rkt")

(provide (struct-out automaton-graph)
         nfa->graph
         dfa->graph)

(struct automaton-graph (states start accept edges) #:transparent)

(define (label-key l) (if (eq? l 'epsilon) 0 1))

(define (label-to-chars l)
  (cond
    [(eq? l 'epsilon) '()]
    [(char? l) (list l)]
    [(set? l) (sort (set->list l) char<?)]
    [else l]))

(define (label<? a b)
  (define ka (label-key a))
  (cond
    [(< ka (label-key b)) #t]
    [(> ka (label-key b)) #f]
    [else
     (let loop ((as (label-to-chars a)) (bs (label-to-chars b)))
       (cond
         [(and (null? as) (null? bs)) #f]
         [(null? as) #t]
         [(null? bs) #f]
         [(< (char->integer (car as)) (char->integer (car bs))) #t]
         [(> (char->integer (car as)) (char->integer (car bs))) #f]
         [else (loop (cdr as) (cdr bs))]))]))

(define (edge<? a b)
  (define fa (car a)) (define fb (car b))
  (define la (cadr a)) (define lb (cadr b))
  (define ta (caddr a)) (define tb (caddr b))
  (cond
    [(< fa fb) #t]
    [(> fa fb) #f]
    [(label<? la lb) #t]
    [(label<? lb la) #f]
    [(< ta tb) #t]
    [else #f]))

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
