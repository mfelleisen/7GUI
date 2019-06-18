## Using Macros to Simplify the Specification of Views and Models 

This directory re-implements the "7 GUIs" task with the help of Racket's
Macro system. The primitive implementation in the top-level directory make
barely use of macros, but their implementation suggests two repeated ideas
that are not expressible in plain Racket. 

- `7guis`: provides macros for specifying
  - the layout of hierarchical GUIs: `gui` and `define-gui`
    An auxiliary `#:change` element in `gui` and `define-gui` directly
    expressed how a widget affects the state of the model. 

  - state variables: `(define-state x"id v:expr  pf:expr)`, which propagates a change 
    to `x` via the propagation function `pf` (to the rest of the model and the view)

- `task 1`: shrinks to a one-line model and a three-line GUI: see `#:change` and `define-state` for data flow 
- `task 2`: the bi-directional change demands a feature for stopping state-change propagation 
- `task 3`: is the first task to benefit from one-line class definitions: 
 ```
  (define stext% (class text-field% (init e) (super-new [label ""][init-value DATE0][enabled e])))
 ```
- `task 4`: --
- `task 5`: it could benefit from macros within the DSL of GUIs
- [`task 6`](task-6.rkt) illustrates how a single program can use the gui layout macros in
  several places and how state variables show up both in the model and the
  view, which of course just means that "model" comes in several layers. 
- [`task 7`](task-7.rkt) is an example of a state variable with complex content
  (a hash table) and a complex update behavior. A change propagates several
  ("many") values when a variable is changed.

Curiously, the injection of `gui`, `define-gui`, and `define-state` into
the code base has exposed a couple of small logical mistakes in the
original code base because the code's intention have become clearer than
the original ones. 

When I reported this to [Michael Ballantyne](http://mballantyne.net), he
replied with the pithy slogan that

	 "you are far more likely to discover bugs by porting a program from plain
	  JavaScript to React than TypeScript. And the same holds for rewriting from
	  a recursive-descent parser in a parsing framework than Typed Racket."

All I can say to this is "take that, Typists!". 

### To Do

- task-5 might benefit from macro-expansion inside of `gui` and
  `define-gui`, specifically the `option` class

```
(define-syntax-rule (but% f lbl) (button% #:change *data (f lbl) [label (format "~a" 'lbl)]))
```

and then we get the 3 horizontal buttons like this:

```
(but% just Create) (but% mk-changer Update) (but% mk-changer Delete)  
```
But this could also be overkill. 
