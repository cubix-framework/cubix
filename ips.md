In `cubix-sui-move` package there is Modularized syntax that was
generated based on Sui Move tree-sitter grammar definiton, found here:
@tree-sitter-sui-move/vendor/tree-sitter-move/external-crates/move/tooling/tree-sitter/grammar.js
I've prepered a starting point for the Incremental Parametric Syntax
modules `Cubix.Language.SuiMove.IPS`,
`Cubix.Language.SuiMove.IPS.Types` and
`Cubix.Language.SuiMove.IPS.Trans`. Your job is to in order, go over
proposed list of nodes that I want to have translated to cubix
parametric syntax and define `Trans` and `Untrans` instance, for
those.

While doing so I want you to make sure that the definitions are
correct by running test target `unit-tests`. There are 2 test modules:
- `TransRoundtripSpec` that checks if `untranslate . translate` is the same
  as original syntax tree.
  It can be run by:
  ```sh
  cabal test --test-option="-m Trans/Untrans Roundtrip Tests"
  ```
- `BannedConstructorsSpec` that checks if the `translate` successfully
  replaced required constructors.  Be sure to run test suite after
  each node replacement, not just at the very end.
  It can be run by:
  ```sh
  cabal test --test-option="-m Banned Constructors Tests"
  ```

In order to define translation from Modularized to Parametric syntax
you'll have to do following changes to the
`Cubix.Language.SuiMove.IPS.Types` and
`Cubix.Language.SuiMove.IPS.Trans` modules:

1. Identify modularized syntax node that can be replaced by parametric
   syntax node. Within that Node identify the constructors that can be
   replaced by the ones from parametric node. It is often the case you
   might not be able to replace node completely.

2. Define sort injection types by utilizing Template Haskell
   functions: `createSortInclusionTypes`, `deriveAllButSortInjection`
   and `createSortInclusionInfers`. If in the step 1, you've
   determined that not all constructors are going to be replaced
   you'll have to define 2 sort injections: Modularized is Parametric
   **and** Parametric is Modularized.

3. Update the `Declaring the IPS` section of
   `Cubix.Language.SuiMove.IPS.Types` with appropriate nodes:
  - add defined sort injection to `suiSortInjections` array
  - add parametric node to list of additional nodes
  - if node is going to be replaced completly it should be removed
    from the Modularized syntax, by subtracting it from the signature
    list of names

4. Define helper function `trans${Node}` that not only translates node
   to parametric one, but alse changes the label to the one from
   parametric syntax. **Important:** When constructing new terms use
   Cubix smart constructors.

5. Define `Trans` instance for node being replaced. If the node
   being replaced is sum type the `trans` from `Trans` class
   definition should be a thin wrapper over helper functions that
   handles single variant. For example look at the
   `Cubix.Language.Salidity.IPS.Trans` module from `cubix-solidity`
   package. In there `Trans` for `Expression` node matches on the type
   of expressions and passes on work to helper functions:
   `translateUnary` or `translateBinary`.

6. Add all the constructors that are translated to `bannedCons` list
   in the `BannedConstructorTH` module to make sure they won't appear
   in translated Parametric syntax tree.

7. Run the `BannedConstructorSpec` test to check if translation is
   defined properly. Make sure to be running only this test as the
   `TransRoundtripSpec` will fail at this moment. Do not move further
   until the `BannedConstructorSpec` tests are passing.

Once the `BannedConstructorSpec` tests are passing you can move to
defining `Untrans` instance. In order to do that:
1. Add sort injections introduced in
   `Cubix.Language.SuiMove.IPS.Types` module to exemption list for
   generating default instance. 
2. Define `untrans${Node}` helper function that also changes the sort
   type, the same as in the case of `Trans`.
3. Define `Untrans` instance. Similarly as in the case for `Trans`, if
   the node is a sum type make sure each variant is handled by helper
   function.

After defining both `Trans` and `Untrans` instances verify if they
work correctly by running the `RountTripSpec` tests.

See my notes on cubix: @../../../org/notes/work/uptospeed/20250611T112029--cubix__plt_work.org

Belowe are the nodes to be translated. Make sure to work them one by
one, only when one is fully implemented move to the next one:

| Modularized node | Parametric node                                                        |
|------------------|------------------------------------------------------------------------|
| BinaryExpression | different possible nodes, but the ones that can produce BinaryOpL sort |
| Block            | Block                                                                  |

Make sure that the project is compiling after each step.