#lang racket

(require rackunit
         "../automata-lens/charset.rkt")

(test-case "a single character renders as the escaped character"
  (check-equal? (charset->string '(#\a)) "a")
  (check-equal? (charset->string '(#\.)) "\\.")
  (check-equal? (charset->string '(#\*)) "\\*"))

(test-case "the full ascii set renders as a dot"
  (check-equal? (charset->string ascii-chars) "."))

(test-case "a small class renders as a range"
  (check-equal? (charset->string '(#\a #\b #\c)) "[a-c]")
  (check-equal? (charset->string '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)) "[0-9]"))

(test-case "a large class renders as its complement"
  (define others
    (filter (lambda (c) (not (member c '(#\a #\b #\c)))) ascii-chars))
  (check-equal? (charset->string others) "[^a-c]"))

(test-case "an epsilon label renders as the epsilon glyph"
  (check-equal? (render-label 'epsilon) "ε"))

(test-case "char-set labels and atom lists render the same way"
  (check-equal? (render-label (list->set '(#\a #\b))) "[a-b]")
  (check-equal? (render-label '(#\a #\b)) "[a-b]"))

(test-case "compress merges adjacent characters into ranges"
  (check-equal? (compress '(#\a #\b #\c #\e #\g #\h))
                (list (cons #\a #\c) (cons #\e #\e) (cons #\g #\h))))

(test-case "a class containing a lone dash stays readable"
  (check-equal? (charset->string '(#\- #\a)) "[\\-a]"))

(test-case "escape-literal escapes metacharacters and control characters"
  (check-equal? (escape-literal #\a) "a")
  (check-equal? (escape-literal #\.) "\\.")
  (check-equal? (escape-literal #\-) "\\-")
  (check-equal? (escape-literal #\nul) "\\x0"))
