#ifdef DEBUG
#include <stdio.h>
#endif
typedef _Bool bool;
typedef struct __attribute__((packed))
        { char x; short y; }
        T;
typedef struct { bool b1; short b2; bool b3;
                 bool b4; short b5; bool b6; }
                 Bools;
union u1 {
  T x;
  /* packed for struct/union fields: smallest possible alignment, i.e. do not add padding zeros to align the pointer
     in an array of u1s */
  __attribute__((packed)) T* y;
  Bools z;
};

union  __attribute__((packed))
u2
{
  T x,*y; Bools z;
};

/* this attribute is ignored, because it belongs to x  */
__attribute__((packed))
union u3
{
  T x, *y; Bools z;
} x;

struct s {
  struct k { short b1 : 8, b2: 9, b3: 8, b4 : 7;} x;
  union u1 a,*b;
  union u2 c_1,c_2;
  union u3 d_1,d_2,d_3;
} __attribute__((packed));

int main()
{
  #ifdef DEBUG
    printf("T: %lu\n", sizeof(T));
    printf("Bools: %lu\n", sizeof(Bools));
    printf("struct k: %lu\n", sizeof(struct k));
    printf("struct s: %lu\n", sizeof(struct s));
    printf("union u1: %lu\n", sizeof(union u1));
    printf("union u2: %lu\n", sizeof(union u2));
    printf("union u3: %lu\n", sizeof(union u3));
  #endif
}
