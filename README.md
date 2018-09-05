# cubix
The Cubix framework for multi-language transformation. Explained in the OOPSLA 2018 paper "One Tool, Many Languages: Language-Parametric Transformation with Incremental Parametric Syntax"


# Compilation notes

With -O1 or -O2, compilation takes forever.

We recommend using the following command to build Cubix:

    alias stackfastbuild="stack build --ghc-options='-O0 -j +RTS -A256m -n2m -RTS'"

This builds Cubix in parallel with minimal optimization, and sets the initial GHC heap to larger than usual.

We found the following two minimal sets of compilation flags that mitigate this blowup and make compilation manageable:

1: -fno-cse -fno-full-laziness
2: -fno-specialize -funfolding-creation-threshold=0

If disable everything except CSE and specialise, blow-up still occurs. Remains true with -O1
