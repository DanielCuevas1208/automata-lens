#lang racket
(require rackunit
         "../automata-lens/regex.rkt"
         "../automata-lens/nfa.rkt"
         "../automata-lens/dfa.rkt"
         "../automata-lens/minimize.rkt"
         "../automata-lens/match.rkt")

(provide corpus
         class-corpus
         make-automata
         all-strings
         dfa-test-alphabet
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

(define class-corpus
  (list
   (list "[a-c]" (list (cons "a" #t) (cons "b" #t) (cons "c" #t)
                       (cons "d" #f) (cons "" #f)))
   (list "[0-9]+" (list (cons "0" #t) (cons "123" #t) (cons "12a" #f) (cons "" #f)))
   (list "[a-z]+[0-9]" (list (cons "abc1" #t) (cons "a" #f) (cons "abc" #f)))
   (list "a.b" (list (cons "acb" #t) (cons "aab" #t) (cons "a1b" #t)
                     (cons "ab" #f) (cons "a" #f)))
   (list "[^a]b" (list (cons "bb" #t) (cons "cb" #t) (cons "ab" #f) (cons "b" #f)))
   (list "^a$" (list (cons "a" #t) (cons "ab" #f) (cons "b" #f)))
   (list "a?[bc]" (list (cons "a" #f) (cons "ab" #t) (cons "b" #t)
                        (cons "c" #t) (cons "bc" #f)))))

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

(define (dfa-test-alphabet dfa)
  (remove-duplicates (append* (dfa-alphabet dfa)) eq?))

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
