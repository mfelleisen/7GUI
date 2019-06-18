## Adding Types to the Primitive GUI Solution 

The files in this directory re-implement the "7 GUIs" task in Typed Racket,
using the  power of *migratory* typing. 

- `task 1`: a single type annotation for a callback function suffices 
- `task 2`: needs some type definitions and types for a widget element generator
   - `from-string`:  needs a simple string to exact integer converter module, 
     which Typed Racket should just provide 
- `task 3`: like task 2
  - `gregor`: needs an adapter module for a library (whose code we don't want to re-write)
- `task 4`: `timer%` needs a type surprisingly 
- `task 5`: a macro generates typed functions! 

So far so good. 

- `task 6`: needs two new modules:
  - `sub-frame`: a module that abstracts over the type of `frame%` for sub-typing the class
  - `sub-canvas`: a module that abstracts over the type of `canvas%` for sub-typing the class
- `task 7`: needs the two new modules plus the adapter modules for 
  - `task-7-exp`: TODO I think I should be able to eliminate Letter 
  - `task-7-view`: showcases the simplicity of writing adapters 
  - `double-click-canvas`: Typed Racket deals well with augmentation! (see below)

It is because of the type system's expressive power that we just need type
adapter modules for these three files (which are inessential to the tasks
or should have been provided by Racket's base library).

## The Experience 

From the design perspective, adding types was less useful than exploiting
more of Racket's macro power. See [Macros/README](../Macros/README.md). But
it wasn't useless. 

From the "gradual typing" perspective, the experience started at amazing
high points but eventually deteriorated into one of pure pain for a number
of reasons. 

### DESIGN Issues With My Original Code 

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

### GRADUAL TYPING: Easy Type Injections 

Injection types into tasks 1 through 5 is easy. 

The addition of types typically requires

- a change to the language line of the module 
- a small number of type annotations

Even though, the type checker is of little help with the conversion.  When
the #lang line is changed to typed/racket/gui, the type checker tends to
request type information about the gui elements, defined at the bottom of
the program. As the converted programs show, however, adding types to the
callback functions suffices to make the type checker happy ... and this
doesn't seem to take more effort than a handful of lines.

I encountered two exceptions to this rule: 

1. I introduced a typed adapter module for `gregor`, which is not
surprising for a library. 

2. I added another adapter module, `from-string`, for converting strings to
"rational" numbers. We should probably have more of those for Typed Racket. 

### GRADUAL TYPING: Non-trivial Type Injection 

Converting `task-6` and `task-7` demonstrate in many ways that type
injection is **really truly hard**. The Typed Racket documentation is much
less helpful than the Racket one: 

- it lacks the definitions of types 
- it lacks signature definitions for functions 

Use `(:type f)` instead to figure out the type of `f` and work with
this. It isn't a perfect replacement for documented type signatures.

- It lacks explanations of unusual ideas about types (say, that a Class
  type is really a sub-classing specification). 

  *Note* A Class type is a specification for sub-classing. So when a Class
   type includes an `init` specification for, say, `label` it cannot be used
   with `super-new` --- because that makes it unavailable to its sub-class. 

### Two Important Ideas That Help With Adding Types 

When it came to porting `task-6` and `task-7`, I wasn't particularly clever
about my work. The ease of porting `task-1` to `task-5` had lulled me into
thinking that it was going to be a breeze. 

So, the very moment when you change `typed/racket/gui' and `drracket` says
"you have 35 type errors", switch to "clever working mode", and here are
two important hints for this. 

#### Move Self-Contained Pieces of Code to a Separate Buffer 

Moving from Racket to Typed Racket demands the conversion of entire
modules. But, due to various factors, expansion plus type checking is
relatively slow. At the same time, it may take many rounds of
experimentation to get the types right for some expression. 

[[ In the research world, I'd say Typed Racket's "macro" gradual typing
becomes inconvenient for certain tasks. "Micro" gradual typing is clearly
superior for the conversion task, except that it is easy to end up with
types that express much less than the programmer has in mind. ]]

Break out self-contained pieces of code and deal with them in a separate,
temporary file. 

Here is an example from `task-7` that took me forever to get right: 

```
(define-syntax-rule (define-getr name : ResultType HashType (*source selector default))
  (begin
    (: name (-> Letter Index ResultType))
    (define (name letter index) ; (source *source) (result->type selector))
      (define f ((inst hash-ref Ref HashType) *source (list letter index) #f))
      (if f (selector f) default))))
```

This macro comes with several uses at distinct types, and getting
`hash-ref` to work properly here took quite some work.  For example, you
might think that `#f` could be replaced with a proper value of type
`HashType` so that the last line could be replaced with

``` 
     (selector f)
```

but I did not get this to work (in Typed-land. See Macro-land for how to do
this properly.) 

So to get this to work, I copied the macro, its uses, its free variables,
the `#lang` line and all the `require` lines into a separate window. This
sped up the fixing project tremendously. 

#### How to Develop a Class Type for Classes Derived from Racket GUI Classes

When you derive a class from one of Racket's built-in classes and you need
a type for this derived class, things get complicated. They get especially
complicated for GUI widget classes, because they come with dozens of
`init' parameters, fields, and methods. 

For adding types to `task-6`, I discovered the need for `Class` types for
`circle-canvas%` and `adjuster-dialog%`, the classes derived from `canvas%`
and `frame%`, respectively. Take a look at the result: 

- [sub-frame](sub-frame.rkt), which exports a macro for a defining a Class
  type for sub-classes of `frame%`

- [sub-canvas](sub-canvas.rkt), which exports a macro for a defining a Class
  type for sub-classes of `canvas%`

Here is what you need to know. A Class type specifies the interface for
sub-classing. While the `#:implements` clause takes care of inherited
methods (and fields), initial parameters and initial fields need to be
specified again.  See Asumu's comment in [sub-canvas](sub-canvas.rkt) for
an explanation. 

Eventually I figured out a good way to produce this `Class` type. 

- first, use `:type` to retrieve the specifications of the super-class:

```
> (:type frame%) 
(Class ...)
```

- second, copy and paste the init parts of the type into `drracket`

- third, subtract the init parameters and init fields that your derived
  class supplies via `super-new` because they are no longer available for
  configuration from outside the class (well, ...) 

- fourth, add type specifications for the newly introduced public methods
  (and fields). 

If you anticipate to re-use this type again, abstract over it with a
macro. See [sub-canvas](sub-canvas.rkt) for an example. The sample macro
simply adds type specification clauses to the `Class` type and gives a name
to the resulting type. For other uses, I might make the macro subtract
class init parameters and fields, but I haven't had a need for this
functionality yet. 

A small issue came up when I expanded on the gradual approach to typing: 
It turns out that Typed Racket demands a duplicate specification of `pubment` methods:

```
(define-type-canvas Canvas-Double-Click%
  (on-click (-> Natural Natural Void))
  (on-double-click (-> Natural Natural Void))

  [augment (on-click (-> Natural Natural Void))]
  [augment (on-double-click (-> Natural Natural Void))])
```

See [double-click-canvas](double-click-canvas.rkt) for details. 

### Issues With Typed Racket 

1. Our `match-define` does not deal with type annotations on pattern
variables. I opened an issue on this (#829). 

2. Typed Racket's compose can't deal with multiple argument functions: 

```
((inst compose Number Number Number) add1 (λ (x y) (+ x y)))
```

3. It turns out that adding augmentable method (see above) to the `canvas%`
class uncovered missing method type specifications: 
```
(define-type-canvas Canvas-Double-Click%
  (vert-margin (->* () (Integer) Void))
  (horiz-margin (->* () (Integer) Void))
  (get-scaled-client-size (-> (Values Integer Integer)))
  (get-gl-client-size (-> (Values Integer Integer)))
  ...)
```

  [augment (on-event  (-> (Instance Mouse-Event%) Void))]


### An Additional Insight About Typed Racket

It is pretty cool how macros inside of a to-be-converted module can be made
to work with types: 

```
(define-syntax-rule (def-cb (name {x : T}) exp ...)
  (define (name {x : (U String T)} {_y : Any}) : Void exp ... (send lbox set *selected)))
```

