
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

//#define TRACE
//#define TRACE_FINAL

#define die { printf("die: %s:%d (%s), cycles = %d\n", __FILE__, __LINE__, __FUNCTION__,cycles); exit(1); }

typedef uint8_t u8;

static void interpreter_loop();
static u8* read_file(char* filename);
void show(void);

inline static u8 get(void);
inline static void put(u8 b);

static unsigned cycles = 0;
static u8 mem[30000];
static u8* prog;

#define ip instruction_pointer
#define mi memory_index

static unsigned short mi = 0;
static u8* ip;
static u8 current_instr;

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
    case ',': mem[mi] = get(); break;
    case '.': put(mem[mi]); break;
    case '+': ++mem[mi]; break;
    case '-': --mem[mi]; break;
    case '>': ++mi; break;
    case '<': --mi; break;
    case '[': {
      unsigned nesting;
      if (!mem[mi]) {
        for (nesting = 1; nesting>0;) {
          switch (*++ip) {
          case '[': ++nesting; break;
          case ']': --nesting; break;
          }
        }
      }
      break;
    }
    case ']': {
      unsigned nesting;
      if (mem[mi]) {
        for (nesting = 1; nesting>0;) {
          switch (*--ip) {
          case '[': --nesting; break;
          case ']': ++nesting; break;
          }
        }
      }
      break;
    }
    }
    ++ip;
#ifdef TRACE_FINAL
    ++cycles;
#endif
  }
}

u8 get(void) {
  u8 c = getchar();
  if (c == 0xFF) c = 0;
  //printf("getchar() -> %02X\n", c);
  return c;
}

void put(u8 b) {
  putchar(b);
}

void show() {
  printf("cyc=%2d : ip=%2ld, i='%c', mp=%2d -- ", cycles, (ip-prog), *ip, mi);
  for (unsigned i = 0; i < 8; ++i) printf(" %02X", mem[i]);
  printf("\n");
}

u8* read_file(char* filename) {
  FILE* f = fopen (filename, "rb");
  if (!f) die;
  fseek (f, 0, SEEK_END);
  long length = ftell (f);
  fseek (f, 0, SEEK_SET);
  u8* buffer = malloc (length);
  if (fread (buffer, 1, length, f)) {}
  fclose (f);
  return buffer;
}
