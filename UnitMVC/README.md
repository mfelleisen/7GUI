
### Adding Primitive MVC 

These files add a unit-based MVC setup to the ones in the home directory.
Each file specifies 

- a signature-based agreement between model and view 
- a model unit 
- a view unit 
- at least one setup for linking the two 

That is, the files set up mutually recursive, statically linked view and
model units. It is thus possible to link any view to the model as long as
it satisfies the signature and vice versa. See 

- task-1.rkt for a demo of how to abstract functionally over a view unit
  and generate two distinct views for the counter model 


