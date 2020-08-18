int main ()
{
  lab: 1; // 4
  {
    __label__ local_lab, local_lab2, lab;
  int foo() {
    __label__ local_lab;
    goto local_lab; // 22
    goto local_lab2; //29
    goto lab; // 36
    local_lab: 7; // 24
  }
  local_lab2:; // 31
  local_lab: 6; // 61
  lab:; //38
  }

  goto lab; // 80
}
