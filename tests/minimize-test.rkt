#lang racket

(require rackunit
         "../automata-lens/dfa.rkt"
         "../automata-lens/minimize.rkt"
         "../automata-lens/match.rkt"
         "support.rkt")

(define (distinguishable? dfa s1 s2)
  (define n (length (dfa-states dfa)))
  (define (accepting-after state str)
    (let loop ((s state) (cs (string->list str)))
      (if (null? cs)
          (member s (dfa-accept dfa))
          (let ((atom (char-symbol dfa (car cs))))
            (if (not atom)
                #f
                (loop (hash-ref (hash-ref (dfa-trans dfa) s) atom) (cdr cs)))))))
  (for*/or ([len (in-range 0 (add1 n))]
            [str (in-list (filter (lambda (s) (= (string-length s) len))
                                  (all-strings (dfa-test-alphabet dfa) n)))])
    (not (eq? (accepting-after s1 str) (accepting-after s2 str)))))

(test-case "minimization reduces the dfa of a(b|c)*"
  (define-values (nfa dfa min-dfa) (make-automata "a(b|c)*"))
  (void nfa)
  (check-true (< (length (dfa-states min-dfa)) (length (dfa-states dfa))))
  (check-equal? (length (dfa-states dfa)) 5)
  (check-equal? (length (dfa-states min-dfa)) 3))

(test-case "minimization reduces the dfa of a*"
  (define-values (nfa dfa min-dfa) (make-automata "a*"))
  (void nfa)
  (check-true (< (length (dfa-states min-dfa)) (length (dfa-states dfa))))
  (check-equal? (length (dfa-states min-dfa)) 1))

(test-case "minimization keeps the language of every corpus entry"
  (for ([entry (in-list corpus)])
    (check-language! (car entry) (cadr entry))))

(test-case "minimization keeps the language of every character class entry"
  (for ([entry (in-list class-corpus)])
    (check-language! (car entry) (cadr entry))))

(test-case "minimized dfa states are pairwise distinguishable"
  (for ([entry (in-list corpus)])
    (define-values (nfa dfa min-dfa) (make-automata (car entry)))
    (void nfa) (void dfa)
    (define states (dfa-states min-dfa))
    (for* ([i (in-range (length states))]
           [j (in-range (add1 i) (length states))])
      (check-true (distinguishable? min-dfa (list-ref states i) (list-ref states j))
                  (format "'~a' states ~a and ~a are equivalent"
                          (car entry) (list-ref states i) (list-ref states j))))))

(test-case "minimization is idempotent"
  (for ([entry (in-list corpus)])
    (define-values (nfa dfa min-dfa) (make-automata (car entry)))
    (void nfa) (void dfa)
    (define again (minimize-dfa min-dfa))
    (check-equal? (length (dfa-states again)) (length (dfa-states min-dfa))
                  (format "'~a'" (car entry)))
    (for ([str (in-list (all-strings (dfa-test-alphabet min-dfa) 4))])
      (check-eq? (run-result-matched (run-dfa again str))
                 (run-result-matched (run-dfa min-dfa str))
                 (format "'~a' on '~a'" (car entry) str)))))
