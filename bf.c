
#include <stdio.h>
#include <stdlib.h>

//#define TRACE
//#define TRACE_FINAL

#define die { printf("die: %s:%d (%s), cycles = %d\n", __FILE__, __LINE__, __FUNCTION__,cycles); exit(1); }

typedef unsigned char byte;

static void interpreter_loop();
static byte* read_file(char* filename);
void show(void);
static byte get(void);
static void put(byte b);

static unsigned cycles = 0;
static byte mem[30000];
static byte* prog;

#define ip instruction_pointer
#define mp memory_pointer

static byte* mp = &mem[0];
static byte* ip;
static int nesting;
static byte current_instr;

int main(int argc, char* argv[]) {
  if (argc != 2) die;
  prog = read_file(argv[1]);
  interpreter_loop();
#ifdef TRACE_FINAL
  printf("final number of cycles = %d\n", cycles);
#endif
}

void interpreter_loop() {
  ip = prog;
  while ((current_instr = *ip)) {
#ifdef TRACE
    show();
#endif
    switch (current_instr) {
    case ',': *mp = get(); break;
    case '.': put(*mp); break;
    case '+': ++*mp; break;
    case '-': --*mp; break;
    case '>': ++mp; break;
    case '<': --mp; break;
    case '[':
      if (!*mp) {
        for (nesting = 1; nesting>0;) {
          switch (*++ip) {
          case '[': ++nesting; break;
          case ']': --nesting; break;
          }
        }
      }
      break;
    case ']':
      if (*mp) {
        for (nesting = 1; nesting>0;) {
          switch (*--ip) {
          case '[': --nesting; break;
          case ']': ++nesting; break;
          }
        }
      }
      break;
    }
    ++ip;
#ifdef TRACE_FINAL
    ++cycles;
#endif
  }
}

byte get(void) {
  byte c = getchar();
  if (c == 0xFF) c = 0;
  //printf("getchar() -> %02X\n", c);
  return c;
}

void put(byte b) {
  putchar(b);
}

void show() {
  printf("cyc=%2d : ip=%2ld, i='%c', mp=%2ld -- ", cycles, (ip-prog), *ip, (mp-mem));
  for (unsigned i = 0; i < 8; ++i) printf(" %02X", mem[i]);
  printf("\n");
}

byte* read_file(char* filename) {
  FILE* f = fopen (filename, "rb");
  if (!f) die;
  fseek (f, 0, SEEK_END);
  long length = ftell (f);
  fseek (f, 0, SEEK_SET);
  byte* buffer = malloc (length);
  if (fread (buffer, 1, length, f)) {}
  fclose (f);
  return buffer;
}
