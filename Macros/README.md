
### Using Macros to Simplify the Specification of Views and Models 

7guis` provides macros for specifying

- the layout of hierarchical GUIs: `gui` and `define-gui`
  - including an auxiliary `#:change` element for saying more directly how a widget affects the state of the model

- state variables: `define-state`
  -  how a change to the variable propagates to the rest of the model and the view


[task-6](task-6.rkt) illustrates how a single program can use the gui layout macros in
several places and how state variables show up both in the model and the
view, which of course just means that "model" comes in several layer. 

[task-7](task-7.rkt) is an example of a state variable with complex content
(a hash table) and a complex update behavior. A change propagates several
("many") values when a variable is changed.

The injection of `gui`, `define-gui`, and `define-state` into the code base
has exposed a couple of small mistakes in the original code base (some
fixes are backported some are not). 

When I reported this to [Michael Ballantyne](http://mballantyne.net), he
replied with the pithy slogan that

	 "you are far more likely to discover bugs by porting a program from plain
	  JavaScript to React than TypeScript. And the same holds for rewriting from
	  a recursive-descent parser in a parsing framework than Typed Racket."

All I can say to this is "take that, Typists!". 
