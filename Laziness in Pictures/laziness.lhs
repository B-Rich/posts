Haskell, like a few other functional programming languages, evaluates values
lazily -- essentially, when they are first needed.  But what does that mean
really?  What are "strict" and "non-strict" functions?  How do you force
evaluation in a Haskell program, and why does that matter?  These are the
questions I'd like to examine in some depth, using pictures to help us along
the way.

First, let's define a few terms.  A function is **strict** if its arguments
must have well-defined values at the time it is called.  For example, each
boolean passed to a strict logical and operator must either have the values
`True` or `False`.  They cannot be anything else.  This type of thinking
should be pretty comfortable to most imperative programmers.

Where non-strictness comes into play, in any language, is when you have to
reason about values that are not yet determined.  Consider the following C++
code:

```c
bool foo();
bool c = foo() && foo();
```

Although the type of the function `foo` is known at compile-time, the value of
each invocation of `foo` is not known until runtime.  It's possible `foo` may
crash, or enter an infinite loop; what's its value then?  We know it must be
of type of bool in order for the program to compile, but it may or may not have
a well-defined boolean value in the context of the above expression.  It could
be true, false, or some undefined behavior dependent on external circumstances (like
running out of memory).  To express ill-defined values (e.g., not-true and
not-false), we use the term "bottom", written as `⊥`.  It can mean "undefined" or
"non-termination".

In an imperative language, encountering undefined values is almost always detrimental
to the code in some way, since such values are never expected.  In a lazy
language, however, we can still reason with `⊥`.  This is because Haskell has
non-strict semantics: `⊥` is not an error unless it is evaluated.  If it's never
evaluated, it can passed around and around and no one will be the wiser.  Let
me show you what I mean:

> foo :: [(String, String)]
> foo = [("Alpha", "1"), ("Beta", "2"), (undefined, "3")]
>
> bar :: ((String, String) -> a) -> [a]
> bar = let xs = foo in flip map xs
>
> baz = bar snd

In this contrived example, we have a function `bar` which, when given a
function, maps it over a predefined assocation list.  We then pass it `snd` to
extract the second member of each pair in that list.  Note how the third
element uses undefined for its first member.  This is how we explicitly indicate `⊥`
in Haskell code.  It means we don't have a value for that `String` yet.