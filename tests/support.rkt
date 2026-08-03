#lang racket
(require rackunit
         "../automata-lens/regex.rkt"
         "../automata-lens/nfa.rkt"
         "../automata-lens/dfa.rkt"
         "../automata-lens/minimize.rkt"
         "../automata-lens/match.rkt")

(provide corpus
         make-automata
         all-strings
         check-language!)

(define corpus
  (list
   (list "a" (list (cons "a" #t) (cons "b" #f) (cons "" #f)))
   (list "a|b" (list (cons "a" #t) (cons "b" #t) (cons "c" #f) (cons "" #f)))
   (list "a*" (list (cons "" #t) (cons "a" #t) (cons "aaa" #t) (cons "b" #f)))
   (list "ab" (list (cons "ab" #t) (cons "a" #f) (cons "b" #f) (cons "abc" #f)))
   (list "a(b|c)*" (list (cons "a" #t) (cons "ab" #t) (cons "abc" #t)
                         (cons "abcb" #t) (cons "ac" #t)
                         (cons "b" #f) (cons "" #f)))
   (list "(ab|a)*b" (list (cons "b" #t) (cons "ab" #t) (cons "aab" #t)
                          (cons "abab" #t) (cons "ababb" #t)
                          (cons "a" #f) (cons "aba" #f)))
   (list "a+b" (list (cons "ab" #t) (cons "aab" #t) (cons "b" #f) (cons "aa" #f)))
   (list "a?b" (list (cons "b" #t) (cons "ab" #t) (cons "aab" #f)))
   (list "a*|b" (list (cons "" #t) (cons "a" #t) (cons "aa" #t)
                      (cons "b" #t) (cons "ab" #f)))))

(define (make-automata regex-str)
  (define nfa (build-nfa (parse-regex regex-str)))
  (define dfa (nfa->dfa nfa))
  (define min-dfa (minimize-dfa dfa))
  (values nfa dfa min-dfa))

(define (all-strings alphabet max-len)
  (define (power n)
    (if (= n 0)
        '(())
        (for*/list ([prefix (in-list (power (sub1 n)))]
                    [c (in-list alphabet)])
          (cons c prefix))))
  (append* (for/list ([n (in-range 0 (add1 max-len))])
             (map list->string (power n)))))

(define (check-language! regex-str expected-lst)
  (define-values (nfa dfa min-dfa) (make-automata regex-str))
  (for ([pair (in-list expected-lst)])
    (define str (car pair))
    (define expected (cdr pair))
    (check-eq? (nfa-accepts? nfa str) expected
               (format "nfa '~a' on '~a'" regex-str str))
    (check-eq? (run-result-matched (run-dfa dfa str)) expected
               (format "dfa '~a' on '~a'" regex-str str))
    (check-eq? (run-result-matched (run-dfa min-dfa str)) expected
               (format "minimized '~a' on '~a'" regex-str str))))
