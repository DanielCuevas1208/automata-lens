#lang racket

(require rackunit
         "../automata-lens/regex.rkt"
         "../automata-lens/nfa.rkt"
         "support.rkt")

(test-case "nfa accepts the expected language for every corpus entry"
  (for ([entry (in-list corpus)])
    (define-values (nfa dfa min-dfa) (make-automata (car entry)))
    (void dfa) (void min-dfa)
    (for ([pair (in-list (cadr entry))])
      (check-eq? (nfa-accepts? nfa (car pair)) (cdr pair)
                 (format "nfa '~a' on '~a'" (car entry) (car pair))))))

(test-case "nfa states are consecutive integers starting at zero"
  (for ([entry (in-list corpus)])
    (define-values (nfa dfa min-dfa) (make-automata (car entry)))
    (void dfa) (void min-dfa)
    (check-equal? (nfa-states nfa) (range (length (nfa-states nfa)))
                  (format "states of '~a'" (car entry)))))

(test-case "nfa accepts the empty string exactly when epsilon is in the language"
  (check-true (nfa-accepts? (build-nfa (parse-regex "a*")) ""))
  (check-true (nfa-accepts? (build-nfa (parse-regex "()")) ""))
  (check-false (nfa-accepts? (build-nfa (parse-regex "a")) ""))
  (check-false (nfa-accepts? (build-nfa (parse-regex "ab")) "")))
