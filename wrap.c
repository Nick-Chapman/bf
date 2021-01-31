
#include <stdio.h>
#include <stdlib.h>

typedef unsigned char byte;

inline byte get(void);
inline static void put(byte b);

static byte mem[30000];

//int steps = 0;
//#define trace(n) { steps++; printf ("trace(%d): pc=%d\n",steps,n); }
#define trace(n) {}

#define QUOTEME(M) #M
#define INCLUDE_FILE(M) QUOTEME(M)

int main(int argc, char* argv[]) {
  byte* mp = mem;
  goto prog0;
#include INCLUDE_FILE(GEN)
}

byte get(void) {
  byte c = getchar();
  if (c == 0xFF) c = 0;
  return c;
}

void put(byte b) {
  putchar(b);
}
