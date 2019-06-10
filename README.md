
### A Racket Implementation of [7 GUIs](https://eugenkiss.github.io/7guis/)

The top-level of this repository implements the "7 GUIs" task with the bare
minimum that it took me to get things running and make them reflect what I
was doing. (To my surprise, I never had to resort to unit testing.) 

- task 1: a mouse-click counter 
- task 2: a bi-directional temperature converter 
- task 3: a constrained flight booking choice 
- task 4: a timer 
- task 5: a CRUD MVC 
- task 6: a circle drawer, with undo/redo facility (under-specified)
- task 7: a simple spreadsheet
  - task 7-exp: implement the simplistic Expression language for spreadsheets

To install, 
```
$ raco pkg install https://github.com/mfelleisen/7GUI.git 
```

To run in a shell, 
```
./task-N.rkt 
```

To run, 
```
gracket task-N.rkt
```
for any N. 

### [Macros](Macros/)

The Macros directory shows how to develop macros that help clarify what's
happening in the primitive implementations. using macros also reduces the
length of the implementation, though by a constant amount. For the small
problems, this reduction looks like a lot; for the larger one it is not
worth mentioning. 

Still, reformulating the implementations with better "notation" uncovered a
couple of small bugs. See [README](Macros/README.md).


### TO DO 

- a proper MVC organization 
  - separate pieces 
  - propagate REPL changes to model
- a Typed Racket implementation of the Unit code 
- a unit-based organization of the MVC code with demos of how to replace
  the model or the view 
- a Typed Racket implementation 

### Questions To Be Explored

- is there a framework hidden? 
- is there a embedded DSL hidden? 
- would Syndicate help? 

### BUGS 

- the redo for re-size in circle drawer could be a bug but the
  specification is underwhelming so I am not sure 
