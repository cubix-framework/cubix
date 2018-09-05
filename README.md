# What is Cubix?

Cubix is a framework for language-parametric program transformation,
i.e.: defining a single source-to-source program transformation tool that
can be run on multiple languages. In Cubix, you can write
transformations with a type signature like "This transformation works
for any language that has assignments, loops, and a name-binding
analysis," and instantly get separate tools for C, Java, etc. The goal is to radically reduce the cost of building sophisticated
whole-program refactoring tools by allowing each tool to be built for
a much larger market.

Cubix is based on the idea of *incremental
parametric syntax*, a technique for defining families of
representations of programming languages which share common
components, and for defining them as a small modification to a
pre-existing syntax definition. The name "Cubix" comes from the 2000s
television show "Cubix: Robots for Everyone;" in that show, "Cubix" is
a robot composed of modular pieces that can be reassembled for many purposes.

It currently supports C, Java, JavaScript, Lua, and Python.

The Cubix system itself, and the general incremental parametric syntax
approach, is described in the OOPSLA 2018 paper:

* *One Tool, Many Languages: Language-Parametric Transformation with
  Incremental Parametric Syntax*; James Koppel et al
  
We also recommend reading the following papers to get the necessary
background in generic programming to understand Cubix:

* *Data Types à la Carte*, Wouter Swierstra
* *Compositional Data Types*, Patrick Bahr and Thomas Hvitved

Transformations in Cubix use our `compstrat` library of strategy
combinators. To understand strategy combinators, we recommend the
following paper:

* *The Essence of Strategic Programming*, Ralf Lämmel et al

# What Cubix is not

Cubix is the world's first framework that can build language-parametric
source-to-source transformations. As the first of its kind, it often
gets mistaken for a solution to more familiar problems. In particular,
it is not:

* A tool for translating one language into another. Cubix allows you
  to create a single tool that can transform C programs into better C
  programs and Java programs into better Java programs. It is not
  designed for building tools that can transform C programs into Java programs. Indeed, much of
  its power comes from its ability to preserve all the information of
  the original program.
* A collection of ready-to-use refactoring tools. Thus far, all
  transformations built on Cubix are tech demos. While a couple are
  theoretically useful, they have not undergone the amount of UX
  engineering needed to actually be time-savers.
 * A framework for writing multi-language program analyses. Cubix transformations may consume results
   provided by other analyses, which may be written in either a
   single-language or multi-language fashion.
 * A framework for analysis/transformation of polyglot programs, i.e.:
   programs (or single source files) written in multiple
   languages. 
   
However, Cubix's generic-programming capabilities make it a powerful
tool for building all kinds of programming tools, even for only one
language. We are particularly excited about the potential of extending Cubix to
transform polyglot programs.

# Getting started

To build Cubix:

First, download the sub-libraries `comptrans` and `compstrat`

    git submodule update
    
Second, build Cubix:

    stack build --ghc-options='-O0 -j +RTS -A256m -n2m -RTS'"

You may be prompted for your Github credentials to download the
third-party frontends.

You are now ready to run Cubix transformations:

    stack exec examples-multi java hoist input-files/java/Foo.java

# Compilation notes

Because of performance problems in GHC, the full Cubix will not build
with -O1 or -O2. We've tried on a server with 64GB RAM; the server ran
out of memory. Instead, use this command:

    alias stackfastbuild="stack build --ghc-options='-O0 -j +RTS -A256m -n2m -RTS'"

This builds Cubix in parallel with minimal optimization, and sets the initial GHC heap to larger than usual.

We found the following two minimal sets of compilation flags that mitigate this blowup and make compilation manageable:

1: -fno-cse -fno-full-laziness
2: -fno-specialize -funfolding-creation-threshold=0

If disable everything except CSE and specialise, blow-up still
occurs. Remains true with -O1

Adding "--flag cubix:only-one-language" to the build command will turn on a compile flag that disables building support for all languages except Lua, the smallest language. This greatly speeds compilation times, to the point where we are able to compile with -O2. Some of the performance reports in cubix/benchmarks/reports were compiled with this flag.



# Directory Overview

Overview of directories (corresponding to the top-level of the zip file, and the /cubix directory on the Docker image):
/stack.yaml         # High-level build description
/package.yaml     # Low-level build description 
/examples           # Example transformations built with Cubix
/examples/multi # The main driver for the multi-language transformations
/comptrans         # Source code for the comptrans library
/compstrat          # Source code for compstrat, our library for Strategic Programming with Compositional Datatypes
/input-files         # Small test inputs in each of the 5 languages
/scripts               # Scripts for running the transformations over compiler test suites

# Running the built-in transformations and analyses

Use this command from the "languages" directory (the one that contains "stack.yaml"):

    stack exec examples-multi
    
You will be give the following help:

    Usage:
    examples-multi <language> <transform> <file>*
    examples-multi <language> <analysis>  <file>*
    Transforms available: debug,id,cfg,elementary-hoist,hoist,tac,testcov,ipt
    Analyses available: anal-triv-call
    
Note that only the IPT transformation can be run on multiple files.

For example, to run the three-address code transformation on a JavaScript file named "Foo.js", run: `stack exec examples-multi javascript tac Foo.js`
    
It can be faster to run the executable directory, without using `stack exec`. On the first author's laptop, this executable is located at  `.stack-work/dist/x86_64-osx/Cabal-1.22.5.0/build/examples-multi/examples-multi`.

# Using the Interprocedural Plumbing Transformation (IPT) Tool

We recommend also using the `rlwrap` command to enable command history.

Example:

    rlwrap stack exec examples-multi java ipt input-files/java/ipt/*.java

In C and Java, you will additionally be prompted for the type of the
parameter to add. Give this type as an AST for `language-c` or
`language-java` (e.g.: `(PrimType IntT)`, not `int`).
