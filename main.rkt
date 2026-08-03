#lang racket
(require "automata-lens/regex.rkt"
         "automata-lens/nfa.rkt"
         "automata-lens/dfa.rkt"
         "automata-lens/minimize.rkt"
         "automata-lens/match.rkt"
         "automata-lens/graph.rkt"
         "automata-lens/draw.rkt")

(define (run-describe regex-str input-str)
  (define rx (parse-regex regex-str))
  (define nfa (build-nfa rx))
  (define dfa (nfa->dfa nfa))
  (define min-dfa (minimize-dfa dfa))
  (define result (run-dfa dfa input-str))
  (printf "Expression: ~a\n" regex-str)
  (printf "Parsed:     ~a\n" (regex->string rx))
  (printf "NFA states: ~a\n" (length (nfa-states nfa)))
  (printf "DFA states: ~a\n" (length (dfa-states dfa)))
  (printf "Minimized:  ~a states\n" (length (dfa-states min-dfa)))
  (printf "Input:      ~a\n" input-str)
  (printf "~a\n" (trace->string result)))

(define (run-export regex-str path)
  (define rx (parse-regex regex-str))
  (define nfa (build-nfa rx))
  (define dfa (nfa->dfa nfa))
  (export-graph->png (dfa->graph dfa) path)
  (printf "Wrote ~a (~a DFA states)\n" path (length (dfa-states dfa))))

(define (run-export-min regex-str path)
  (define rx (parse-regex regex-str))
  (define nfa (build-nfa rx))
  (define dfa (nfa->dfa nfa))
  (define min-dfa (minimize-dfa dfa))
  (export-graph->png (dfa->graph min-dfa) path)
  (printf "Wrote ~a (~a minimized states)\n" path (length (dfa-states min-dfa))))

(define (usage)
  (displayln "Automata Lens")
  (displayln "  racket main.rkt                        open the desktop application")
  (displayln "  racket main.rkt -d <regex> <input>     describe and trace a match")
  (displayln "  racket main.rkt -e <regex> <file>      export the DFA as a PNG")
  (displayln "  racket main.rkt -m <regex> <file>      export the minimized DFA as a PNG"))

(define (main argv)
  (match (vector->list argv)
    [(list "-d" regex-str input-str) (run-describe regex-str input-str)]
    [(list "-e" regex-str path) (run-export regex-str path)]
    [(list "-m" regex-str path) (run-export-min regex-str path)]
    [_ (usage)
       ((dynamic-require "automata-lens/gui.rkt" 'run-gui))]))

(module+ main
  (main (current-command-line-arguments)))
