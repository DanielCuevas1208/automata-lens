#lang racket

(require rackunit racket/file racket/draw
         "../automata-lens/dfa.rkt"
         "../automata-lens/graph.rkt"
         "../automata-lens/draw.rkt"
         "support.rkt")

(test-case "rendering a dfa produces a non-empty bitmap"
  (define-values (nfa dfa min-dfa) (make-automata "a(b|c)*"))
  (void nfa) (void min-dfa)
  (define bmp (render-graph->bitmap (dfa->graph dfa)))
  (check-true (> (send bmp get-width) 0))
  (check-true (> (send bmp get-height) 0)))

(test-case "rendering with a highlight does not fail"
  (define-values (nfa dfa min-dfa) (make-automata "a(b|c)*"))
  (void nfa) (void min-dfa)
  (define g (dfa->graph dfa))
  (define bmp (render-graph->bitmap g
                                     #:highlight-edge (list (dfa-start dfa) #\a 1)
                                     #:highlight-state 1))
  (check-true (> (send bmp get-width) 0)))

(test-case "rendering an nfa does not fail"
  (define-values (nfa dfa min-dfa) (make-automata "a(b|c)*"))
  (void dfa) (void min-dfa)
  (define bmp (render-graph->bitmap (nfa->graph nfa)))
  (check-true (> (send bmp get-width) 0)))

(test-case "export writes a valid png file"
  (define-values (nfa dfa min-dfa) (make-automata "a(b|c)*"))
  (void nfa) (void min-dfa)
  (define tmp (make-temporary-file "~a.png"))
  (export-graph->png (dfa->graph dfa) tmp)
  (check-true (file-exists? tmp))
  (check-true (> (file-size tmp) 0))
  (define reloaded (read-bitmap tmp))
  (check-true (is-a? reloaded bitmap%))
  (check-true (> (send reloaded get-width) 0))
  (delete-file tmp))

(test-case "export writes a png for the minimized dfa"
  (define-values (nfa dfa min-dfa) (make-automata "a*|b"))
  (void nfa) (void dfa)
  (define tmp (make-temporary-file "~a.png"))
  (export-graph->png (dfa->graph min-dfa) tmp)
  (check-true (> (file-size tmp) 0))
  (delete-file tmp))
