#lang racket

(provide ascii-chars
         ascii-set
         render-label
         charset->string
         ranges->string
         compress
         escape-literal)

(define ascii-chars
  (build-list 128 (lambda (i) (integer->char i))))

(define ascii-set (list->set ascii-chars))

(define (compress sorted-chars)
  (if (null? sorted-chars)
      '()
      (let loop ((chars (cdr sorted-chars))
                 (lo (car sorted-chars))
                 (hi (car sorted-chars))
                 (acc '()))
        (cond
          [(null? chars) (reverse (cons (cons lo hi) acc))]
          [(= (char->integer (car chars)) (add1 (char->integer hi)))
           (loop (cdr chars) lo (car chars) acc)]
          [else
           (loop (cdr chars) (car chars) (car chars)
                 (cons (cons lo hi) acc))]))))

(define (range-text range)
  (if (= (char->integer (car range)) (char->integer (cdr range)))
      (escape-in-class (car range))
      (string-append (escape-in-class (car range))
                     "-"
                     (escape-in-class (cdr range)))))

(define (ranges->string ranges)
  (apply string-append (map range-text ranges)))

(define (control? c)
  (or (< (char->integer c) 32) (> (char->integer c) 126)))

(define (escape-literal c)
  (cond
    [(control? c)
     (format "\\x~a" (number->string (char->integer c) 16))]
    [(member c (string->list ".*+?|()[]^$\\-"))
     (string-append "\\" (string c))]
    [else (string c)]))

(define (escape-in-class c)
  (cond
    [(control? c)
     (format "\\x~a" (number->string (char->integer c) 16))]
    [(member c (string->list "]\\^-"))
     (string-append "\\" (string c))]
    [else (string c)]))

(define (charset->string chars)
  (define sorted (sort chars char<?))
  (define n (length sorted))
  (cond
    [(= n 0) ""]
    [(equal? sorted ascii-chars) "."]
    [(= n 1) (escape-literal (car sorted))]
    [else
     (define complement (filter (lambda (c) (not (member c sorted))) ascii-chars))
     (if (<= (length complement) n)
         (string-append "[^" (ranges->string (compress complement)) "]")
         (string-append "[" (ranges->string (compress sorted)) "]"))]))

(define (render-label label)
  (cond
    [(eq? label 'epsilon) "ε"]
    [(char? label) (escape-literal label)]
    [(set? label) (charset->string (set->list label))]
    [(and (pair? label) (andmap char? label)) (charset->string label)]
    [else (format "~a" label)]))
