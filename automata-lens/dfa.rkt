#lang racket
(require "nfa.rkt")

(provide (struct-out dfa)
         nfa->dfa)

(struct dfa (states start accept alphabet trans) #:transparent)

(define (nfa->dfa nfa)
  (define edges (nfa-edges nfa))
  (define alphabet
    (sort (remove-duplicates
           (for*/list ([(s lst) (in-hash edges)]
                       [e (in-list lst)]
                       #:when (not (eq? (car e) 'epsilon)))
             (car e)))
          char<?))
  (define (canonical states) (sort (remove-duplicates states) <))
  (define (closure states) (epsilon-closure edges states))
  (define (move-on states c) (move states edges c))

  (define set->id (make-hash))
  (define id->set (make-hash))
  (define (register! states)
    (define canon (canonical states))
    (or (hash-ref set->id canon #f)
        (let ((id (hash-count set->id)))
          (hash-set! set->id canon id)
          (hash-set! id->set id canon)
          id)))

  (define start-id (register! (closure (list (nfa-start nfa)))))
  (define dead-id (register! '()))

  (define (id-of states)
    (define canon (canonical states))
    (or (hash-ref set->id canon #f)
        (if (null? canon) dead-id (register! canon))))

  (define trans (make-hash))
  (let loop ((pending (list start-id)))
    (unless (null? pending)
      (define next-round '())
      (for ([id (in-list pending)])
        (when (not (hash-has-key? trans id))
          (define nfa-set (hash-ref id->set id))
          (define row (make-hash))
          (hash-set! trans id row)
          (for ([c (in-list alphabet)])
            (define target (id-of (closure (move-on nfa-set c))))
            (hash-set! row c target)
            (when (not (hash-has-key? trans target))
              (set! next-round (cons target next-round))))))
      (loop (reverse next-round))))

  (define states (sort (hash-keys trans) <))
  (define accepts
    (sort (for/list ([(id nfa-set) (in-hash id->set)]
                     #:when (member (nfa-accept nfa) nfa-set))
            id)
          <))
  (dfa states start-id accepts alphabet trans))
