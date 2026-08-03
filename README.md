# Automata Lens

Automata Lens is a desktop program in Racket. It turns a regular expression
into a finite automaton. It draws the states and animates each match step.

```
Expression -> AST -> NFA -> DFA -> Minimized DFA
```

## What it does

- Parse a small regular expression with character classes.
- Build a nondeterministic automaton (NFA) from the expression.
- Convert the NFA into a deterministic automaton (DFA).
- Minimize the DFA with a before and after view.
- Animate the match path for a test string.
- Export the automaton as a PNG image.

The desktop window shows two panes. The left pane shows the NFA or the DFA.
The right pane shows the minimized DFA. Play walks the input string one
character at a time. It highlights the active state and the edge that leads
to it. The expression language supports classes, ranges, negation, the dot,
and anchors.

## How it works

The pipeline runs in fixed stages. Each stage is a pure function in its own
module.

1. The parser turns the expression into a syntax tree.
2. Thompson construction builds an NFA from the tree.
3. Subset construction converts the NFA into a DFA.
4. Moore partition refinement minimizes the DFA.
5. The matcher records each step for animation.
6. The renderer draws the automaton and the highlights.

The core modules have no dependency on the GUI. The GUI, the image export,
and the command line share the same core. This design keeps the core
testable without a display.

## Repository structure

```
main.rkt                 entry point: GUI and command line
automata-lens/regex.rkt  parser for the expression language
automata-lens/nfa.rkt    NFA construction and simulation
automata-lens/dfa.rkt    subset construction
automata-lens/minimize.rkt  DFA minimization
automata-lens/match.rkt  step-by-step matcher
automata-lens/charset.rkt   label rendering and character ranges
automata-lens/layout.rkt deterministic graph layout
automata-lens/draw.rkt   bitmap and PNG rendering
automata-lens/graph.rkt  shared drawing view of an automaton
automata-lens/gui.rkt    desktop application
tests/                   rackunit test suite
samples/                 sample expressions and generated images
```

## Expression syntax

| Syntax     | Meaning                          |
| ---------- | -------------------------------- |
| `a`        | the literal character a          |
| `ab`       | a followed by b                  |
| `a\|b`     | a or b                           |
| `a*`       | a repeated zero or more times    |
| `a+`       | a repeated one or more times     |
| `a?`       | a or nothing                     |
| `(ab)`     | grouping                         |
| `()`       | the empty string                 |
| `\*`       | the literal character *          |
| `[abc]`    | any character from the class     |
| `[a-c]`    | any character in the range       |
| `[^abc]`   | any character not in the class   |
| `.`        | any ASCII character              |
| `^`        | start anchor, accepted only      |
| `$`        | end anchor, accepted only        |
| `\-`       | the literal character -          |
| `\]`       | the literal character ]          |

Class ranges count all characters in the ASCII table. Matching always covers
the whole string. The anchors mark the ends and never change the result.

## Setup

Install Racket 9 or newer from the official download page. The standard
distribution includes the required GUI and drawing packages.

Open a terminal in the project directory.

Run the desktop application:

```
racket main.rkt
```

Describe a match on the command line:

```
racket main.rkt -d "[a-z]+" racket
```

Export the DFA as a PNG image:

```
racket main.rkt -e "a(b|c)*" automaton.png
```

Export the minimized DFA as a PNG image:

```
racket main.rkt -m "a(b|c)*" automaton-minimized.png
```

Install the package for project-wide use:

```
raco pkg install --auto .
```

Run the test suite:

```
raco test tests/run-all.rkt
```

## Sample output

The describe command prints the automaton sizes and the match trace.

```
Expression: a(b|c)*
Parsed:     a(b|c)*
Symbols:    a b c
NFA states: 10
DFA states: 5
Minimized:  3 states
Input:      abcb
Matched: #t
  step 0: start -> state 0
  step 1: a -> state 2
  step 2: b -> state 3
  step 3: c -> state 4
  step 4: b -> state 3
```

The samples directory contains the generated images. See
`samples/automaton.png` for the DFA and `samples/automaton-minimized.png`
for the minimized DFA. The files `automaton-class.png`,
`automaton-dot.png`, `automaton-negated.png`, and `automaton-anchored.png`
show the character class, the dot, the negated class, and the anchors.
`samples/patterns.txt` lists the sample expressions and their inputs.

## Limitations

The expression language stays small on purpose. It has no search mode, no
shorthand escapes, and no repetition ranges. Literals are single
characters. The tool matches the whole input string, so the anchors never
change the result. The desktop window needs a graphical display. Image
export works without a window.

## Test status

The test suite passes in this release. CI runs it on every push. Seven test
files cover the parser, the NFA, the DFA, minimization, matching, charset
rendering, and image rendering.

## Roadmap

Complete:

- Character classes, ranges, negation, the dot, and anchors.

Remaining:

- Add syntax-error highlighting in the GUI.
- Add a textual table of states and transitions.
- Support saving and loading expressions.
- Add repetition counts such as `{2,4}`.
- Add shorthand escapes such as `\d` and `\w`.

## License

MIT. See the LICENSE file.
