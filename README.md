
## A Racket Implementation of [7 GUIs](https://eugenkiss.github.io/7guis/)

The purpose of this repository is to use the "seven GUI challenge" to
illustrate basic GUI programming in Racket and, more importantly, explore
program transformations such as the injection of types, macros, or both.

The top-level of this repository implements the "7 GUIs" task with the bare
minimum that it took me to get things running and make them reflect what I
was doing. (To my surprise, I never had to resort to unit testing.) 

- `task 1`: a mouse-click counter 
- `task 2`: a bi-directional temperature converter 
- `task 3`: a constrained flight booking choice 
- `task 4`: a timer 
- `task 5`: a CRUD MVC 
- `task 6`: a circle drawer, with undo/redo facility (under-specified)
- `task 7`: a simple spreadsheet
  - `task 7-exp`: implement the simplistic Expression language for spreadsheets
  - `task-7-view`: draw a grid on to some drawing context, and then add content 
  - `canvas-double-click`: a canvas with methods for single-click and double-click reactions
    [should perhaps be fleshed out and part of the GUI library]

To install, 
```
$ raco pkg install https://github.com/mfelleisen/7GUI.git 
```

To run in a shell, 
```
$ ./task-N.rkt 
```

*Pasteboard and Snips*

The basic Racket control and canvas toolkit suffices for the seven GUIs
challenges. Working through these examples provides a good first impression
of its power, though for a thorough explanation I recommend the Racket
documentation. 

For sophisticated GUI applications, Racket supports pasteboard and snip
widgets. Alex Harsányi has written up a [beautiful
introduction](https://alex-hhh.github.io/2018/10/chess-game-using-racket-s-pasteboard.html)
on this topic. 

### [Macros](Macros/)

The `Macros/` directory shows how to develop macros that help clarify what's
happening in the primitive implementations. using macros also reduces the
length of the implementation, though by a constant amount. For the small
problems, this reduction looks like a lot; for the larger one it is not
worth mentioning. 

Still, reformulating the implementations with better "notation" uncovered a
couple of small bugs. See [README](Macros/README.md).


### [Types](Types/)

The `Types/` directory demonstrates what it takes to add types to the
simple implementations. The overhead is small for five of the seven tasks,
non-trivial for the other two. 

The use of macros to define families of type families is a great
illustration of how Racket tools compose in a powerful synthesis. 

The addition of types points to small inconsistencies and revealed one
misconception about a callback. 

It also brought home that we need a guide for program conversions. 

### TO DO 

- a proper MVC organization 
  - separate pieces 
  - propagate REPL changes to model
- a unit-based organization of the MVC code with demos of how to replace
  the model or the view 

- a unit-based organization .. with types 

### Questions To Be Explored

- is there a framework hidden? 
- is there a embedded DSL hidden? 
- would Syndicate help? 

### BUGS 

- the redo for re-size in circle drawer could be a bug but the
  specification is underwhelming so I am not sure 

### Acknowledgments 

[Jun 20, 2019] Will Byrd reported three bugs: 

- [typos](https://github.com/mfelleisen/7GUI/commit/f90261a6790ed34f08afeb42f33e1fa646e7b543)
- [geometry mismanagement](https://github.com/mfelleisen/7GUI/commit/c83ca4ccdbbc8e665019825c3280f9d5c003e146)
- ['reading' numeric information from a text-field$](https://github.com/mfelleisen/7GUI/commit/13f00394789c21ae5dd9dd5bda003d449cdaf1f7)

Asumu Takikawa explained `Class` again and how `init` works. 

Ben Greenman discovered that `augment` methods also need `public`
specifications in `Class`. 

Sam Tobin-Hochstadt assisted with some non-Class-y aspects. 


For other small assists, see the [commit
log](https://github.com/mfelleisen/7GUI/commits/master). 
