
#include <stdio.h>
#include <stdlib.h>

static int trace = 0;
static unsigned cycles = 0;

#define die { printf("die: %s:%d (%s), cycles = %d\n", __FILE__, __LINE__, __FUNCTION__,cycles); exit(1); }

typedef unsigned char byte;

byte* read_file(char* filename) {
  FILE* f = fopen (filename, "rb");
  if (!f) die;
  fseek (f, 0, SEEK_END);
  long length = ftell (f);
  fseek (f, 0, SEEK_SET);
  byte* buffer = malloc (length);
  fread (buffer, 1, length, f);
  fclose (f);
  return buffer;
}

static byte get(void) {
  byte c = getchar();
  if (c == 0xFF) c = 0;
  if (trace) printf("getchar() -> %02X\n", c);
  return c;
}

static void put(byte b) {
  putchar(b);
}

int main(int argc, char* argv[]) {
  byte mem[30000];
  if (argc != 2) die;
  byte* prog = read_file(argv[1]);
  byte* ip = prog;
  byte* mp = &mem[0];
  byte instr;
  for (; (instr = *ip); ++ip, ++cycles) {
    if (trace) {
      printf("cyc=%2d : ip=%2ld, i='%c', mp=%2ld -- ",
             cycles, (ip-prog), *ip, (mp-mem));
      for (unsigned i = 0; i < 8; ++i) printf(" %02X", mem[i]);
      printf("\n");
    }
    switch (instr) {
    case ',': *mp = get(); break;
    case '.': put(*mp); break;
    case '+': ++*mp; break;
    case '-': --*mp; break;
    case '>': ++mp; break;
    case '<': --mp; break;
    case '[':
      if (!*mp) {
        for (int n = 1; n>0;) {
          switch (*++ip) {
          case '[': ++n; break;
          case ']': --n; break;
          }
        }
      }
      break;
    case ']':
      if (*mp) {
        for (int n = 1; n>0;) {
          switch (*--ip) {
          case '[': --n; break;
          case ']': ++n; break;
          }
        }
      }
      break;
    }
  }
  printf("final number of cycles = %d\n", cycles);
}
