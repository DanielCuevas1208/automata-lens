#lang racket

(require rackunit
         "../automata-lens/dfa.rkt"
         "../automata-lens/match.rkt"
         "support.rkt")

(test-case "run-dfa records one step per input character plus the start"
  (define-values (nfa dfa min-dfa) (make-automata "a(b|c)*"))
  (void nfa) (void min-dfa)
  (define result (run-dfa dfa "abcb"))
  (check-equal? (length (run-result-steps result)) 5)
  (check-true (run-result-matched result)))

(test-case "the first step is the start state with no character"
  (define-values (nfa dfa min-dfa) (make-automata "a(b|c)*"))
  (void nfa) (void min-dfa)
  (define first (car (run-result-steps (run-dfa dfa "a"))))
  (check-false (trace-step-char first))
  (check-equal? (trace-step-state first) (dfa-start dfa)))

(test-case "trace characters follow the input in order"
  (define-values (nfa dfa min-dfa) (make-automata "a(b|c)*"))
  (void nfa) (void min-dfa)
  (define steps (run-result-steps (run-dfa dfa "abc")))
  (check-equal? (map (lambda (st) (and (trace-step-char st) (string (trace-step-char st))))
                     (cdr steps))
                '("a" "b" "c")))

(test-case "trace indices advance from zero"
  (define-values (nfa dfa min-dfa) (make-automata "a(b|c)*"))
  (void nfa) (void min-dfa)
  (define result (run-dfa dfa "abcb"))
  (check-equal? (map trace-step-index (run-result-steps result)) '(0 1 2 3 4)))

(test-case "a string outside the language is rejected"
  (define-values (nfa dfa min-dfa) (make-automata "a(b|c)*"))
  (void nfa) (void min-dfa)
  (check-false (run-result-matched (run-dfa dfa "x")))
  (check-false (run-result-matched (run-dfa dfa ""))))

(test-case "the empty string matches a star pattern"
  (define-values (nfa dfa min-dfa) (make-automata "a*"))
  (void nfa) (void min-dfa)
  (check-true (run-result-matched (run-dfa dfa "")))
  (check-true (run-result-matched (run-dfa dfa "aaa")))
  (check-false (run-result-matched (run-dfa dfa "aab"))))
