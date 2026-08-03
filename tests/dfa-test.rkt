#lang racket

(require rackunit
         "../automata-lens/dfa.rkt"
         "../automata-lens/match.rkt"
         "support.rkt")

(test-case "dfa agrees with the nfa on every corpus entry"
  (for ([entry (in-list corpus)])
    (check-language! (car entry) (cadr entry))))

(test-case "dfa agrees with the nfa on every character class entry"
  (for ([entry (in-list class-corpus)])
    (check-language! (car entry) (cadr entry))))

(test-case "dfa alphabet holds the distinct atoms in order"
  (define-values (nfa dfa min-dfa) (make-automata "a(b|c)*"))
  (void nfa) (void min-dfa)
  (check-equal? (dfa-alphabet dfa) '((#\a) (#\b) (#\c))))

(test-case "dfa alphabet groups a character class into one atom"
  (define-values (nfa dfa min-dfa) (make-automata "[a-c]|d"))
  (void nfa) (void min-dfa)
  (check-equal? (dfa-alphabet dfa) '((#\a #\b #\c) (#\d))))

(test-case "char-symbol resolves a character to its atom"
  (define-values (nfa dfa min-dfa) (make-automata "[a-c]|d"))
  (void nfa) (void min-dfa)
  (check-equal? (char-symbol dfa #\b) '(#\a #\b #\c))
  (check-equal? (char-symbol dfa #\d) '(#\d))
  (check-false (char-symbol dfa #\z)))

(test-case "dfa transition function is total"
  (for ([entry (in-list corpus)])
    (define-values (nfa dfa min-dfa) (make-automata (car entry)))
    (void nfa) (void min-dfa)
    (for ([s (in-list (dfa-states dfa))])
      (define row (hash-ref (dfa-trans dfa) s))
      (for ([atom (in-list (dfa-alphabet dfa))])
        (check-true (hash-has-key? row atom)
                    (format "'~a' state ~a has no move on ~a"
                            (car entry) s atom))))))

(test-case "dfa and minimized dfa agree on every short string"
  (for ([entry (in-list corpus)])
    (define-values (nfa dfa min-dfa) (make-automata (car entry)))
    (void nfa)
    (for ([str (in-list (all-strings (dfa-test-alphabet dfa) 4))])
      (check-eq? (run-result-matched (run-dfa dfa str))
                 (run-result-matched (run-dfa min-dfa str))
                 (format "'~a' on '~a'" (car entry) str)))))

(test-case "dfa and minimized dfa agree on a small character class"
  (define-values (nfa dfa min-dfa) (make-automata "[a-c]|d"))
  (void nfa)
  (for ([str (in-list (all-strings '(#\a #\b #\c #\d) 4))])
    (check-eq? (run-result-matched (run-dfa dfa str))
               (run-result-matched (run-dfa min-dfa str))
               (format "'[a-c]|d' on '~a'" str))))

(test-case "a single-character pattern builds a three-state dfa"
  (define-values (nfa dfa min-dfa) (make-automata "a"))
  (void nfa) (void min-dfa)
  (check-equal? (length (dfa-states dfa)) 3))
