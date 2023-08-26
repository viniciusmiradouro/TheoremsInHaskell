# TheoremsInHaskell

This repo is meant to store my explorations using Haskell as a proof checker
based on intuitionistic logic that follows the "Propositions as types - Proofs
as programs" paradigm.

With that said, theorem statements here are the type signature of functions and
the definitions of their bindings are their proofs or, using intuitionistic
lingo, their construction.

If you'd like to contribute, take notice that I'm trying to use as little
external packages as possible, but sensible ones that help with this process,
like singletons-base, which gives us some dependent typing tools which help us
express complicated quantified statements, are welcome.

# Setup

The usual. Make sure you have ```stack```, ```ghcup``` and
```stylish-haskell``` installed, move to the root of the repo and run ```stack
build```.

# Misc

Even while working with ```stylish-haskell```, some formatting is open to the
author. So, regarding style, use your best judgement based on what is already
on the repo.
