int main ()
{
  goto foo;
  int foo() {
    goto foo;
    foo: ;
  }

 foo: 5;
}
