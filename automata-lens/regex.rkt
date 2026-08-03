#lang racket

(provide parse-regex
         regex->string
         (struct-out rx-char)
         (struct-out rx-empty)
         (struct-out rx-concat)
         (struct-out rx-union)
         (struct-out rx-star))

(struct rx-char (ch) #:transparent)
(struct rx-empty () #:transparent)
(struct rx-concat (left right) #:transparent)
(struct rx-union (left right) #:transparent)
(struct rx-star (body) #:transparent)

(define stop-chars '(#\| #\)))

(define (parse-regex str)
  (define chars (string->list str))
  (define (peek) (if (pair? chars) (car chars) #f))
  (define (next!)
    (when (pair? chars)
      (set! chars (cdr chars))))
  (define (parse-union)
    (let loop ((acc (parse-concat)))
      (if (eq? (peek) #\|)
          (begin
            (next!)
            (loop (rx-union acc (parse-concat))))
          acc)))
  (define (parse-concat)
    (let loop ((acc #f))
      (if (or (null? chars) (memq (peek) stop-chars))
          (or acc (rx-empty))
          (let ((piece (parse-repeat)))
            (loop (if acc (rx-concat acc piece) piece))))))
  (define (parse-repeat)
    (let loop ((acc (parse-atom)))
      (case (peek)
        [(#\*) (next!) (loop (rx-star acc))]
        [(#\+) (next!) (loop (rx-concat acc (rx-star acc)))]
        [(#\?) (next!) (loop (rx-union acc (rx-empty)))]
        [else acc])))
  (define (parse-atom)
    (define c (peek))
    (cond
      [(not c) (error 'parse-regex "unexpected end of expression")]
      [(eq? c #\() (next!)
       (define inner (parse-union))
       (unless (eq? (peek) #\))
         (error 'parse-regex "expected a closing parenthesis"))
       (next!)
       inner]
      [(eq? c #\)) (error 'parse-regex "unexpected closing parenthesis")]
      [(eq? c #\\) (next!)
       (define escaped (peek))
       (unless escaped
         (error 'parse-regex "backslash at the end of the expression"))
       (next!)
       (rx-char escaped)]
      [else (next!) (rx-char c)]))
  (define result (parse-union))
  (when (pair? chars)
    (error 'parse-regex (format "unexpected character: ~a" (car chars))))
  result)

(define (regex->string rx)
  (match rx
    [(rx-char c) (format "~a" c)]
    [(rx-empty) "()"]
    [(rx-concat l r) (string-append (regex->string l) (regex->string r))]
    [(rx-union l r) (format "(~a|~a)" (regex->string l) (regex->string r))]
    [(rx-star b) (format "~a*" (regex->string b))]))
