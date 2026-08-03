#lang racket

(require rackunit "../automata-lens/regex.rkt")

(define literal rx-char)
(define empty rx-empty)
(define concat rx-concat)
(define union rx-union)
(define star rx-star)

(test-case "parse a literal and a concat"
  (check-equal? (parse-regex "a") (literal #\a))
  (check-equal? (parse-regex "ab") (concat (literal #\a) (literal #\b))))

(test-case "parse a union"
  (check-equal? (parse-regex "a|b") (union (literal #\a) (literal #\b))))

(test-case "union has lower precedence than concat"
  (check-equal? (parse-regex "ab|cd")
                (union (concat (literal #\a) (literal #\b))
                       (concat (literal #\c) (literal #\d)))))

(test-case "parse a star"
  (check-equal? (parse-regex "a*") (star (literal #\a))))

(test-case "plus and optional are sugar"
  (check-equal? (parse-regex "a+") (concat (literal #\a) (star (literal #\a))))
  (check-equal? (parse-regex "a?") (union (literal #\a) (empty))))

(test-case "parentheses group a subexpression"
  (check-equal? (parse-regex "(ab)*") (star (concat (literal #\a) (literal #\b)))))

(test-case "an empty expression is epsilon"
  (check-equal? (parse-regex "") (empty))
  (check-equal? (parse-regex "()") (empty)))

(test-case "a backslash escapes a metacharacter"
  (check-equal? (parse-regex "a\\*") (concat (literal #\a) (literal #\*))))

(test-case "the printer round-trips a parsed expression"
  (define rx (parse-regex "a(b|c)*"))
  (check-equal? (regex->string rx) "a(b|c)*"))

(test-case "reject malformed input"
  (check-exn exn:fail? (lambda () (parse-regex "(")))
  (check-exn exn:fail? (lambda () (parse-regex "a)")))
  (check-exn exn:fail? (lambda () (parse-regex "a(b")))
  (check-exn exn:fail? (lambda () (parse-regex "a\\")))
  (check-exn exn:fail? (lambda () (parse-regex "a|)b"))))

(test-case "digits and letters are literals"
  (check-equal? (parse-regex "a1") (concat (literal #\a) (literal #\1))))
