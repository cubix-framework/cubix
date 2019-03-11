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

* 
*One Tool, Many Languages: Language-Parametric Transformation with
  Incremental Parametric Syntax*; James Koppel et al](http://www.jameskoppel.com/files/papers/oopsla18main-p221-p.pdf)
  
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
    
Cubix has many dependencies, several of which are not on Hackage/Stackage, including some forks of Hackage libraries whose changes have not been merged upstream. This makes it more difficult to create a new package which depends on Cubix. For an example of how to do this, see https://github.com/jkoppel/using-cubix-example].

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

# Running the language test suites

Cubix comes with scripts for running any semantics-preserving
transformation on language test suites for C, Java, JavaScript, Lua,
and Python, in the files `scripts/test_java.rb` and similar. To do so:

  1. Follow the below instructions to install the language tests.
  2. Each .rb file contains a constant at the top like `JAVA_DIR` and
  `JAVA_TESTS` pointing to the language implementation and tests
  directory. Modify these appropriately.
  3. Run `./scripts/test_<lang>.rb <name of transformation>`

These scripts will run the transformation on all tests, run the
transformed tests, and report the final pass/fail counts.

For all transformations except `id`, they will first run the identity
transformation, and discard any tests that fail. This rules out tests
that trigger bugs in the 3rd party parsers/pretty-printers, most (but
not all) self-referential tests, and tests that the original language
implementation fails.

They also come with a special `count_loc` parameter that counts the
total lines of code in all relevant tests.


## Installing GCC and GCC torture


*Note*: The C instructions are still being updated.

In some directory:

    git clone https://github.com/gcc-mirror/gcc.git
    cd gcc
    
    # If you want the same revision of gcc-torture as in the paper
    git reset --hard f72de674726c5d054b9d99b0a4db09dfb52bf494
    
    cd ..
    mkdir gcc_build
    cd gcc_build
    ../gcc-mirror/configure
    make -j8
    make install

## Installing the K-Java test suite

In some directory:

    git clone https://github.com/kframework/java-semantics
    
    # If you want the same revision as in the paper
    cd java-semantics
    git reset --hard c202266304340a2a4be81fa21ee4fe36b3117ee3
    
    
## Installing test262, the JavaScript spec conformance tests, and the KJS test driver

In some directory:

    git clone https://github.com/kframework/javascript-semantics.git
    cd javascript-semantics
    git reset --hard d5aca308d12d3838c645e1f787e2abc9257ce43e # Only if you want the same revision as in the paper
    make test262

## The Lua tests

As described in the paper, we had to make several modifications to the
Lua test suite to get them to run with Cubix. In particular, we
removed several overly self-referential tests, and modified the test
suite to report the number of passing/total assertions, rather than
aborting the entire suite on the first failure.

These tests are in the `test/lua/lua-5.3.3-tests` directory.

## Installing the CPython tests

In some directory:

    git clone https://github.com/python/cpython.git
    cd cpython
    git reset --hard 7bd4afec86849a57b48f375a9c4e0c32f0539dad # Only if you want the same revision as in the paper
    ./configure
    make
