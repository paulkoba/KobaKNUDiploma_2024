// RUN: %clang -S -emit-llvm -arch arm -march=armv7a %s
typedef unsigned short uint16_t;
typedef __attribute__((neon_vector_type(8))) uint16_t uint16x8_t;

void b(uint16x8_t sat, uint16x8_t luma)
{
  __asm__("vmov.16 %1, %0   \n\t"
                                           "vtrn.16 %0, %1   \n\t"
   :"=w"(luma), "=w"(sat)
   :"0"(luma)
   );

}
