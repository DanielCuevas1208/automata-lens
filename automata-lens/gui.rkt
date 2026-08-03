#lang racket
(require racket/gui
         "regex.rkt" "nfa.rkt" "dfa.rkt" "minimize.rkt" "match.rkt"
         "graph.rkt" "draw.rkt")

(provide run-gui)

(define (run-gui)
  (define frame
    (new frame% [label "Automata Lens"] [width 1100] [height 800]))
  (define main-panel
    (new vertical-panel% [parent frame] [spacing 8]))

  (define row1 (new horizontal-panel% [parent main-panel] [spacing 6]))
  (new message% [parent row1] [label "Expression:"] [min-width 84])
  (define regex-field
    (new text-field% [parent row1] [label ""] [init-value "a(b|c)*"] [min-width 200]))
  (new message% [parent row1] [label "Input string:"] [min-width 90])
  (define input-field
    (new text-field% [parent row1] [label ""] [init-value "abcb"] [min-width 150]))

  (define canvas-row (new horizontal-panel% [parent main-panel] [spacing 8]))
  (define left-pane (new vertical-panel% [parent canvas-row] [spacing 4]))
  (new message% [parent left-pane] [label "Automaton"])
  (define right-pane (new vertical-panel% [parent canvas-row] [spacing 4]))
  (new message% [parent right-pane] [label "Minimized"])

  (define bmp-table (make-hash))
  (define (make-canvas parent)
    (define canvas
      (new (class canvas%
             (super-new)
             (define/override (on-paint)
               (define dc (send this get-dc))
               (send dc set-brush "white" 'solid)
               (send dc set-pen "white" 1 'solid)
               (send dc draw-rectangle 0 0
                     (max 1 (send this get-width))
                     (max 1 (send this get-height)))
               (define bmp (hash-ref bmp-table this #f))
               (if bmp
                   (send dc draw-bitmap bmp 0 0)
                   (begin
                     (send dc set-font (make-object font% 13 'default))
                     (send dc set-text-foreground 'gray)
                     (send dc draw-text "No automaton yet." 14 14)))))
           [parent parent] [min-width 470] [min-height 430]))
    (hash-set! bmp-table canvas #f)
    canvas)

  (define left-canvas (make-canvas left-pane))
  (define right-canvas (make-canvas right-pane))

  (define status
    (new message% [parent main-panel] [label "Ready."] [min-width 400]))

  (define state (make-hash))
  (hash-set! state 'nfa #f)
  (hash-set! state 'dfa #f)
  (hash-set! state 'min #f)
  (hash-set! state 'anim-result #f)
  (hash-set! state 'anim-idx 0)

  (define (show-in! canvas g #:highlight-edge [he #f] #:highlight-state [hs #f])
    (define bmp (render-graph->bitmap g #:highlight-edge he #:highlight-state hs))
    (hash-set! bmp-table canvas bmp)
    (send canvas refresh))

  (define (clear-canvas! canvas)
    (hash-set! bmp-table canvas #f)
    (send canvas refresh))

  (define (left-graph)
    (define view (send view-choice get-selection))
    (define nfa (hash-ref state 'nfa #f))
    (define dfa (hash-ref state 'dfa #f))
    (cond
      [(and (= view 0) nfa) (nfa->graph nfa)]
      [(and (= view 1) dfa) (dfa->graph dfa)]
      [else #f]))

  (define (refresh-views!)
    (define g (left-graph))
    (if g (show-in! left-canvas g) (clear-canvas! left-canvas))
    (define m (hash-ref state 'min #f))
    (if m (show-in! right-canvas (dfa->graph m)) (clear-canvas! right-canvas)))

  (define (build!)
    (with-handlers ([exn:fail? (lambda (e) (send status set-label (exn-message e)))])
      (define rx (parse-regex (send regex-field get-value)))
      (define nfa (build-nfa rx))
      (define dfa (nfa->dfa nfa))
      (define min-dfa (minimize-dfa dfa))
      (hash-set! state 'nfa nfa)
      (hash-set! state 'dfa dfa)
      (hash-set! state 'min min-dfa)
      (hash-set! state 'anim-result #f)
      (refresh-views!)
      (send status set-label
            (format "Built ~a NFA states, ~a DFA states, ~a minimized states."
                    (length (nfa-states nfa))
                    (length (dfa-states dfa))
                    (length (dfa-states min-dfa))))))

  (define (stop!)
    (send anim-timer stop)
    (hash-set! state 'anim-result #f)
    (refresh-views!)
    (send status set-label "Stopped."))

  (define (tick!)
    (define result (hash-ref state 'anim-result #f))
    (define idx (hash-ref state 'anim-idx 0))
    (cond
      [(not result) (send anim-timer stop)]
      [(>= idx (length (run-result-steps result)))
       (send anim-timer stop)
       (send status set-label
             (format "Done. Matched: ~a" (run-result-matched result)))]
      [else
       (define steps (run-result-steps result))
       (define st (list-ref steps idx))
       (define g (dfa->graph (hash-ref state 'dfa #f)))
       (define he
         (and (> idx 0)
              (let* ((prev (list-ref steps (sub1 idx)))
                     (s (trace-step-state prev))
                     (c (trace-step-char st))
                     (t (trace-step-state st)))
                (list s c t))))
       (show-in! left-canvas g
                 #:highlight-edge he
                 #:highlight-state (trace-step-state st))
       (send status set-label
             (format "Step ~a/~a: ~a -> state ~a"
                     idx (length steps)
                     (or (and (trace-step-char st) (format "~a" (trace-step-char st)))
                         "start")
                     (trace-step-state st)))
       (hash-set! state 'anim-idx (add1 idx))]))

  (define anim-timer
    (new timer% [interval 700] [notify-callback tick!]))

  (define (play!)
    (define dfa (hash-ref state 'dfa #f))
    (when (not dfa)
      (send status set-label "Build an automaton first.")
      (void))
    (when dfa
      (when (= (send view-choice get-selection) 0)
        (send view-choice set-selection 1))
      (define result (run-dfa dfa (send input-field get-value)))
      (hash-set! state 'anim-result result)
      (hash-set! state 'anim-idx 0)
      (send anim-timer start)))

  (define (minimize!)
    (define dfa (hash-ref state 'dfa #f))
    (when (not dfa)
      (send status set-label "Build an automaton first.")
      (void))
    (when dfa
      (define min-dfa (minimize-dfa dfa))
      (hash-set! state 'min min-dfa)
      (show-in! right-canvas (dfa->graph min-dfa))
      (send status set-label
            (format "Minimized to ~a states." (length (dfa-states min-dfa))))))

  (define (export!)
    (define g (left-graph))
    (unless g
      (send status set-label "Nothing to export. Build first.")
      (void))
    (when g
      (with-handlers ([exn:fail? (lambda (e) (send status set-label (exn-message e)))])
        (export-graph->png g (send out-field get-value))
        (send status set-label (format "Exported PNG to ~a." (send out-field get-value))))))

  (define view-choice
    (new choice% [parent row1] [label "View"]
         [choices '("NFA" "DFA")] [selection 1]
         [callback (lambda (c e) (refresh-views!))]))

  (define row2 (new horizontal-panel% [parent main-panel] [spacing 6]))
  (new button% [parent row2] [label "Build"]
       [callback (lambda (b e) (build!))])
  (new button% [parent row2] [label "Play"]
       [callback (lambda (b e) (play!))])
  (new button% [parent row2] [label "Stop"]
       [callback (lambda (b e) (stop!))])
  (new button% [parent row2] [label "Minimize"]
       [callback (lambda (b e) (minimize!))])
  (new message% [parent row2] [label "Output file:"])
  (define out-field
    (new text-field% [parent row2] [label ""] [init-value "automaton.png"] [min-width 130]))
  (new button% [parent row2] [label "Export PNG"]
       [callback (lambda (b e) (export!))])

  (send frame show #t)
  (let loop () (sleep/yield 1) (loop)))
