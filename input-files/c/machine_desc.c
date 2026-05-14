/* A C program to generate a "MachineDesc" value for the target
   architecture
*/

#include <stdio.h>

#define size(ty, a) printf ("        " #ty " -> %lu\n", sizeof(a))
#define align(ty, a) printf ("        " #ty " -> %lu\n", _Alignof(a))

int main (){
  printf("md :: MachineDesc\n");
  printf("md =\n");
  printf("  let iSize = \\case\n");
  size(TyBool, _Bool);
  size(TyChar, char);
  size(TySChar, signed char);
  size(TyUChar, unsigned char);
  size(TyShort, short);
  size(TyUShort, unsigned short);
  size(TyInt, int);
  size(TyUInt, unsigned int);
  size(TyLong, long);
  size(TyULong, unsigned long);
  size(TyLLong, long long);
  size(TyULLong, unsigned long long);
  size(TyInt128, __int128);
  size(TyUInt128, unsigned __int128);
  printf("      fSize = \\case\n");
  size(TyFloat, float);
  size(TyDouble, double);
  size(TyLDouble, long double);
  printf("        TyFloatN{} -> error \"TyFloatN\"\n");
  printf("      builtinSize = \\case\n");
  size(TyVaList, va_list);
  printf("        TyAny -> error \"TyAny\"\n");
  printf("      ptrSize = %lu\n", sizeof(void*));
  printf("      voidSize = %lu\n", sizeof(void));
  printf("      iAlign = \\case\n");
  align(TyBool, _Bool);
  align(TyChar, char);
  align(TySChar, signed char);
  align(TyUChar, unsigned char);
  align(TyShort, short);
  align(TyUShort, unsigned short);
  align(TyInt, int);
  align(TyUInt, unsigned int);
  align(TyLong, long);
  align(TyULong, unsigned long);
  align(TyLLong, long long);
  align(TyULLong, unsigned long long);
  align(TyInt128, __int128);
  align(TyUInt128, unsigned __int128);
  printf("      fAlign = \\case\n");
  align(TyFloat, float);
  align(TyDouble, double);
  align(TyLDouble, long double);
  printf("        TyFloatN{} -> error \"TyFloatN\"\n");
  printf("      builtinAlign = \\case\n");
  align(TyVaList, va_list);
  printf("        TyAny -> error \"TyAny\"\n");
  printf("      ptrAlign = %lu\n", _Alignof(void*));
  printf("      voidAlign = %lu\n", _Alignof(void));
  printf("    in  MachineDesc{..}\n");
}
