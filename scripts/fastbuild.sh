#!/bin/bash
stack build --ghc-options='-O0 -j +RTS -A256m -n2m -RTS'
