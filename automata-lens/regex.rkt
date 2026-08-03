#lang racket
(require "charset.rkt")

(provide parse-regex
         regex->string
         (struct-out rx-char)
         (struct-out rx-empty)
         (struct-out rx-concat)
         (struct-out rx-union)
         (struct-out rx-star)
         (struct-out rx-class)
         (struct-out rx-dot)
         (struct-out rx-anchor))

(struct rx-char (ch) #:transparent)
(struct rx-empty () #:transparent)
(struct rx-concat (left right) #:transparent)
(struct rx-union (left right) #:transparent)
(struct rx-star (body) #:transparent)
(struct rx-class (chars negated?) #:transparent)
(struct rx-dot () #:transparent)
(struct rx-anchor (kind) #:transparent)

(define stop-chars '(#\| #\)))

(define (parse-regex str)
  (define chars (string->list str))
  (define (peek) (if (pair? chars) (car chars) #f))
  (define (peek-n n)
    (let loop ((lst chars) (k n))
      (cond
        [(null? lst) #f]
        [(= k 0) (car lst)]
        [else (loop (cdr lst) (sub1 k))])))
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
  (define (parse-class)
    (next!) ; consume '['
    (define negated? (eq? (peek) #\^))
    (when negated? (next!))
    (define (read-item!)
      (define c (peek))
      (cond
        [(not c) (error 'parse-regex "unclosed character class")]
        [(eq? c #\\) (next!)
         (define esc (peek))
         (unless esc (error 'parse-regex "backslash at the end of a class"))
         (next!) esc]
        [else (next!) c]))
    (define chars '())
    (define (push! c) (set! chars (cons c chars)))
    (let loop ()
      (define c (peek))
      (cond
        [(not c) (error 'parse-regex "unclosed character class")]
        [(eq? c #\]) (next!)]
        [else
         (define c1 (read-item!))
         (define after (and (eq? (peek) #\-) (peek-n 1)))
         (if (and after (not (eq? after #\])))
             (let ()
               (next!) ; consume '-'
               (define c2 (read-item!))
               (unless (> (char->integer c2) (char->integer c1))
                 (error 'parse-regex (format "reversed range ~a-~a" c1 c2)))
               (for ([i (in-range (char->integer c1) (add1 (char->integer c2)))])
                 (push! (integer->char i))))
             (push! c1))
         (loop)]))
    (when (and (null? chars) (not negated?))
      (error 'parse-regex "empty character class"))
    (rx-class (remove-duplicates (sort chars char<?)) negated?))

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
      [(eq? c #\[) (parse-class)]
      [(eq? c #\.) (next!) (rx-dot)]
      [(eq? c #\^) (next!) (rx-anchor 'start)]
      [(eq? c #\$) (next!) (rx-anchor 'end)]
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
    [(rx-char c) (escape-literal c)]
    [(rx-empty) "()"]
    [(rx-concat l r) (string-append (regex->string l) (regex->string r))]
    [(rx-union l r) (format "(~a|~a)" (regex->string l) (regex->string r))]
    [(rx-star b) (format "~a*" (regex->string b))]
    [(rx-class chars negated?)
     (string-append (if negated? "[^" "[")
                    (ranges->string (compress chars))
                    "]")]
    [(rx-dot) "."]
    [(rx-anchor kind) (if (eq? kind 'start) "^" "$")]))
(struct regex-parse-error exn:fail (position) #:transparent)
