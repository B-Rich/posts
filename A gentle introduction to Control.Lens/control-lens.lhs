Lenses in Haskell provide a uniform, composable way to access and modify
sub-parts of data structures (where by modify we mean: return a new copy with
that part changed).  In this introduction I will not talk about theory or laws
behind Lenses -- both of which are worthy of study -- but rather how you can
use them to write simpler, more expressive code.

> import Control.Lens           -- HIDE
> import Control.Category
> import Prelude hiding (id, (.))

\## Tuple Lenses

\### A first look: accessing tuples

Lenses look a lot like functions, and they're composable like functions.  The
main difference between them is that you use lenses in conjunction with either
an accessor or a modifier operator, which applies the lens in various way to
the data structure.

For example, the simplest lenses access the members of a tuple.  Two such
lenses are called `_1` and `_2`.  Here is how you'd use them to inspect a
tuple:

    [ghci]
    (1, 2)^._1
      1
    (1, 2)^._2
      2

You can compose lenses with `.`, just as you would functions, with the notable
difference that they compose from left to right, the reverse of functions.
This makes it much easier to figure out which element will ultimately be
accessed:

    [ghci]
    (1, ((2, (3, 4)), 5))^._2._1._2._1
      3

Reading from left to right, this means: Access the second member, then the
first of that, then the second of that, then the first of that.  In non-Lens
code this would read:

    [ghci]
    fst (snd (fst (snd (1, ((2, (3, 4)), 5)))))
      3

But where lenses really shine is modification.  Consider the preceding tuple:
How would you go about replacing the `3` with a `6` in regular Haskell?  You'd
have to use pattern matching to call out the parts you didn't want to modify,
and then build a new tuple around them:

    [ghci]
    let replace1 (a, ((b, (_, d)), e)) f = (a, ((b, (f, d)), e))
    replace1 (1, ((2, (3, 4)), 5)) 6
      (1,((2,(6,4)),5))

Clearly this is not a solution that scales well with deeply nested tuple
structures.

\### Modifying tuples

Another benefit of the lens approach is that accessing and modifying a data
structure uses the *exact same lens*, while accessing and modifying data
structures in Haskell -- such as the tuple above -- often requires very
different code patterns.

To modify our tuple, you first create a setter function from the lens by
calling the `.~` operator with the value you want to replace the element with.
Then call the resulting function on the tuple.  Idiomatically this often looks
like the following:

    [ghci]
    _2._1._2._1 .~ 6 $ (1, ((2, (3, 4)), 5))
      (1,((2,(6,4)),5))

Or you can break it up into many functions, if you wanted:

    [ghci]
    let myLens     = _2._1
    let doubleLens = myLens . myLens
    let mySetter x = doubleLens .~ x
    let myTuple    = (1, ((2, (3, 4)), 5))
    mySetter 6 myTuple
      (1,((2,(6,4)),5))

\## Lists