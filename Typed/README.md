
### Adding Types to the Primitive GUI Solution 

These files add types to the primitive solutions in the home directory. 

The addition of types typically requires 

- a change to the language line of the module 
- a small number of type annotations

Surprisingly the type checker is of little help with the conversion.  When
the #lang line is changed to typed/racket/gui, the type checker tends to
request type information about one of the gui elements (at the bottom of
the program). As the converted programs `task-1` thru `task-5` show,
however, adding types to the callback functions suffices to make the type
checker happy ... and this doesn't seem to take more effort than a handful
of lines (so far).

The exceptions are the need for (1) a typed adapter module for `gregor` and
(2) another one for converting strings to "rational" numbers (`from-string`).

### Non-trivial Type Injection 

Converting `task-6` demonstrated in many ways that type injection is
**hard**. I needed to add two relatively large type definitions for derived
classes: 

- [sub-frame](sub-frame.rkt), which exports a macro for a defining a Class
  type for sub-classes of `frame%`

- [sub-canvas](sub-canvas.rkt), which exports a macro for a defining a Class
  type for sub-classes of `canvas%`

Both conversions were hard. A Class type specifies the interface for
sub-classing. While the `#:implements` clause takes care of inherited
methods, initial parameters and initial fields need to be specified
again. The best way to get there is to use `:type` to retrieve the
specifications of the super-class: 

```
> (:type frame%) 
(Class ...)
```

and to copy-and-paste the init parts of the type into the definitions
area. The above-mentioned files define macros to splice in the additions of
the derived class. 

*Note* A Class type is a specification for sub-classing. So when a Class
 type includes an `init` specification for, say, `label` it cannot be used
 with `super-new` --- because that makes it unavailable to its sub-class. 

### Issues 

Two small issues showed up during the conversion so far: 

1. The type checker could not figure out that my flight-booker program
(task 3) guarantees the existence of a selection in the `*kind` choice
field. Since this isn't always the case, the probe for a selection may
produce `#f` and the types for this method exposed the problem: 

```
((inst list-ref String) CHOICES (if self (or (send self get-selection) 0) 0)))
```

2. Using a variable to record both "the mouse is inside some area and this
is its x coordinate" works well in Untyped code but for Types you're better
off with splitting this into two variables. (It added a variable
declaration and simplified a method, a lot.) 

3. Adding types made me discover that the paint callback in a canvas
receives the drawing context as the second argument, so there's no need to
retrieve it with a `send`. The code wasn't wrong to say the second argument
is `Any`, but figuring out what it was was a "good thing". 

4. I could not figure out how to get an `Exact-Rational` from a string so I
used a `cast`: 

```
(and r (if (real? r) (cast r Exact-Rational) #f)
```
I'll have to dig into this. ~~ **Thanks to Sam TH for helping me fix
this.**

Working on this problem (see `from-string`) also made me realize that my
untyped Celsius converter had no problem dealing with `Complex` Celsius
degrees. And they seem to come out as `Complex` Fahrenheit degrees. Now I
don't know about you, but I have no problems with `Complex` Celsius. But my
types did; so I switched to `Real`. 


5. Our `match-define` does not deal with type annotations on pattern
variables. I opened an issue on this (#829). 

### Insight 

It is pretty cool how macros inside of a to-be-converted module can
manipulate types: 

```
(define-syntax-rule (def-cb (name {x : T}) exp ...)
  (define (name {x : (U String T)} {_y : Any}) : Void exp ... (send lbox set *selected)))
```

