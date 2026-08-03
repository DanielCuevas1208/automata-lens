#lang racket
(require "regex.rkt" "charset.rkt")

(provide (struct-out nfa)
         build-nfa
         nfa-accepts?
         epsilon-closure
         move)

(struct nfa (states start accept edges) #:transparent)

(define (label-set rx)
  (match rx
    [(rx-char c) (set c)]
    [(rx-class chars negated?)
     (define s (list->set chars))
     (if negated? (set-subtract ascii-set s) s)]
    [(rx-dot) ascii-set]
    [_ (error 'build-nfa "node has no character label")]))

(define (build-nfa rx)
  (define counter (box 0))
  (define (fresh!)
    (define id (unbox counter))
    (set-box! counter (add1 id))
    id)
  (define edges (make-hash))
  (define (add-edge! from label to)
    (hash-update! edges from (lambda (lst) (cons (cons label to) lst)) '()))
  (define (build rx)
    (match rx
      [(rx-char c)
       (define s (fresh!))
       (define a (fresh!))
       (add-edge! s (set c) a)
       (values s a)]
      [(rx-class _ _)
       (define s (fresh!))
       (define a (fresh!))
       (add-edge! s (label-set rx) a)
       (values s a)]
      [(rx-dot)
       (define s (fresh!))
       (define a (fresh!))
       (add-edge! s ascii-set a)
       (values s a)]
      [(rx-anchor _)
       (define s (fresh!))
       (values s s)]
      [(rx-empty)
       (define s (fresh!))
       (values s s)]
      [(rx-concat l r)
       (define-values (s1 a1) (build l))
       (define-values (s2 a2) (build r))
       (add-edge! a1 'epsilon s2)
       (values s1 a2)]
      [(rx-union l r)
       (define s (fresh!))
       (define a (fresh!))
       (define-values (s1 a1) (build l))
       (define-values (s2 a2) (build r))
       (add-edge! s 'epsilon s1)
       (add-edge! s 'epsilon s2)
       (add-edge! a1 'epsilon a)
       (add-edge! a2 'epsilon a)
       (values s a)]
      [(rx-star b)
       (define s (fresh!))
       (define a (fresh!))
       (define-values (s1 a1) (build b))
       (add-edge! s 'epsilon s1)
       (add-edge! a1 'epsilon a)
       (add-edge! a1 'epsilon s1)
       (add-edge! s 'epsilon a)
       (values s a)]))
  (define-values (start accept) (build rx))
  (define states
    (sort (remove-duplicates (cons start (cons accept (hash-keys edges)))) <))
  (nfa states start accept edges))

(define (epsilon-closure edges starts)
  (define seen (make-hash))
  (define result '())
  (define (scan! queue)
    (cond
      [(null? queue) (void)]
      [(hash-has-key? seen (car queue)) (scan! (cdr queue))]
      [else
       (define s (car queue))
       (hash-set! seen s #t)
       (set! result (cons s result))
       (scan! (append (cdr queue)
                      (for/list ([e (in-list (hash-ref edges s '()))]
                                 #:when (eq? (car e) 'epsilon))
                        (cdr e))))]))
  (scan! starts)
  (sort result <))

(define (move states edges atom)
  (define rep (car atom))
  (for*/list ([s (in-list states)]
              [e (in-list (hash-ref edges s '()))]
              #:when (and (not (eq? (car e) 'epsilon))
                          (set-member? (car e) rep)))
    (cdr e)))

(define (nfa-accepts? nfa str)
  (define edges (nfa-edges nfa))
  (let loop ((states (epsilon-closure edges (list (nfa-start nfa))))
             (cs (string->list str)))
    (cond
      [(null? cs)
       (and (member (nfa-accept nfa) states) #t)]
      [(null? states) #f]
      [else
       (define next (epsilon-closure edges (move states edges (list (car cs)))))
       (loop next (cdr cs))])))
