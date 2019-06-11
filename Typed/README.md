
### Adding Types to the Primitive GUI Solution 

These files add types to the primitive solutions in the home directory. 

The addition of types typically requires 

- a change to the language line of the module 
- a small number of type annotations

Surprisingly the type checker is of little help with the conversion. 
When the #lang line is changed to typed/racket/gui, the type checker
tends to request type information about one of the gui elements (at the
bottom of the program). As the converted programs show, however, adding
types to the callback functions suffices to make the type checker happy
... and this doesn't seem to take more effort than a handful of lines (so
far). 

The exceptions are the need for (1) a typed adapter module for `gregor` and
(2) another one for converting strings to "rational" numbers (`from-string`).

### Issues 

Two small issues showed up during the conversion so far: 

1. The type checker could not figure out that my flight-booker program
(task 3) guarantees the existence of a selection in the `*kind` choice
field. Since this isn't always the case, the probe for a selection may
produce `#f` and the types for this method exposed the problem: 

```
((inst list-ref String) CHOICES (if self (or (send self get-selection) 0) 0)))
```

2. I could not figure out how to get an `Exact-Rational` from a string so I
used a `cast`: 

```
(and r (if (real? r) (cast r Exact-Rational) #f)
```
I'll have to dig into this. 
