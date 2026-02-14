# Simply Typed Lambda Calculus Extended
Haskell implementation of the Simply Typed Lambda Calculus with a variety of extensions, type checking and inference (algorithm T, as of yet).

## Instructions
The steps for setting up this project are as follows:

```cabal build```

```cabal run```

If warnings appear, fix them! In case you're required to install a missing dependency, it can be accomplished by using a command like:

```cabal install --lib <library_name>```

It's very likely to be a missing dependency - that is, the issue - so this should suffice.

**You need cabal and GHC in order to use this, as you may have already noticed.**

Once you're running the program, write ```:?``` and press enter - it should give you about half the indications you need.

As for the other half, I suggest taking a look at **Benjamin Pierce's "Types and Programming Languages" book**, most particularly its 11th chapter (which pertains to extensions).
