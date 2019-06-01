
### A Racket Implementation of [7 GUIs](https://eugenkiss.github.io/7guis/)

- task 1: a mouse-click counter 
- task 2: a bi-directional temperature converter 
- task 3: a constrained flight booking choice 
- task 4: a timer 
- task 5: a CRUD MVC 
- task 6: a circle drawer, with undo/redo facility (under-specified)
- task 7: a simple spreadsheet


To install, 
```
$ raco pkg install git@...
```

To run, 
```
gracket task-N.rkt
```
for any N. 


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
