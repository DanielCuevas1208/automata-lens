#lang racket
(require "dfa.rkt")

(provide (struct-out trace-step)
         (struct-out run-result)
         run-dfa
         trace->string)

(struct trace-step (index char state accepting?) #:transparent)

(struct run-result (steps matched) #:transparent)

(define (run-dfa dfa str)
  (define trans (dfa-trans dfa))
  (define start (dfa-start dfa))
  (define (accepting? s) (and (member s (dfa-accept dfa)) #t))
  (let loop ((s start) (cs (string->list str)) (i 0)
             (acc (list (trace-step 0 #f start (accepting? start)))))
    (if (null? cs)
        (run-result (reverse acc) (accepting? s))
        (let* ((c (car cs))
               (atom (char-symbol dfa c))
               (next (and atom (hash-ref (hash-ref trans s) atom #f))))
          (if (not next)
              (run-result (reverse acc) #f)
              (let ((step (trace-step (add1 i) c next (accepting? next))))
                (loop next (cdr cs) (add1 i) (cons step acc))))))))

(define (trace->string result)
  (define (step-label st)
    (if (trace-step-char st)
        (format "~a" (trace-step-char st))
        "start"))
  (string-join
   (cons (format "Matched: ~a" (run-result-matched result))
         (for/list ([st (in-list (run-result-steps result))])
           (format "  step ~a: ~a -> state ~a"
                   (trace-step-index st)
                   (step-label st)
                   (trace-step-state st))))
   "\n"))
