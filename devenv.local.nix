{ lib, ... }:

{
  env = {
    "JAVA_TESTS" = lib.mkForce
      "/Users/jkoppel/research_large/other_frameworks/java-semantics/tests";
    "JS_TESTS" = lib.mkForce
      "/Users/jkoppel/research_large/other_frameworks/javascript-semantics/test262/test/suite";
    "JS_PRELUDE" = lib.mkForce
      "/Users/jkoppel/research_large/other_frameworks/javascript-semantics/prelude.js";
    "JS_EXCLUDE" = lib.mkForce
      "/Users/jkoppel/research_large/other_frameworks/java-semantics/list-invalid-tests.txt";
    "PYTHON_TESTS" = lib.mkForce
      "/Users/jkoppel/research_large/other_frameworks/cpython/Lib/test";
    "GCC_TORTURE_TESTS" = lib.mkForce
      "Users/jkoppel/research_large/other_frameworks/gcc/gcc/testsuite/gcc.c-torture/execute";
  };
}
