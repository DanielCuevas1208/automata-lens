#lang racket

(require rackunit
         "../automata-lens/dfa.rkt"
         "../automata-lens/match.rkt"
         "support.rkt")

(test-case "dfa agrees with the nfa on every corpus entry"
  (for ([entry (in-list corpus)])
    (check-language! (car entry) (cadr entry))))

(test-case "dfa alphabet holds the distinct symbols in order"
  (define-values (nfa dfa min-dfa) (make-automata "a(b|c)*"))
  (void nfa) (void min-dfa)
  (check-equal? (dfa-alphabet dfa) '(#\a #\b #\c)))

(test-case "dfa transition function is total"
  (for ([entry (in-list corpus)])
    (define-values (nfa dfa min-dfa) (make-automata (car entry)))
    (void nfa) (void min-dfa)
    (for ([s (in-list (dfa-states dfa))])
      (define row (hash-ref (dfa-trans dfa) s))
      (for ([c (in-list (dfa-alphabet dfa))])
        (check-true (hash-has-key? row c)
                    (format "'~a' state ~a has no move on ~a"
                            (car entry) s c))))))

(test-case "dfa and minimized dfa agree on every short string"
  (for ([entry (in-list corpus)])
    (define-values (nfa dfa min-dfa) (make-automata (car entry)))
    (void nfa)
    (define alphabet (dfa-alphabet dfa))
    (for ([str (in-list (all-strings alphabet 4))])
      (check-eq? (run-result-matched (run-dfa dfa str))
                 (run-result-matched (run-dfa min-dfa str))
                 (format "'~a' on '~a'" (car entry) str)))))

(test-case "a single-character pattern builds a three-state dfa"
  (define-values (nfa dfa min-dfa) (make-automata "a"))
  (void nfa) (void min-dfa)
  (check-equal? (length (dfa-states dfa)) 3))
