#include "main.h"

// -----------------------------------------------------------------------------

int test_add(int a, int b) {
    return a + b;
}

// -----------------------------------------------------------------------------

int const CONSTNUMARR[64] = {
 26840, 19275, 26064, 27828, 57676, 19859, 3947, 25588, 41700, 40697, 40329,
 11102, 6289, 606, 2743, 48741, 1128, 45315, 34812, 19564, 12270, 57406, 51768,
 5209, 51430, 27753, 46415, 55106, 36712, 43261, 3113, 53868, 10325, 51222,
 28607, 38436, 30444, 30914, 21983, 62479, 4819, 34505, 46516, 27770, 58781,
 46802, 19796, 42607, 49886, 26116, 25490, 15969, 6968, 5022, 6152, 2440, 65459,
 42714, 21557, 36048, 61721, 2188, 43148, 19778
};

int test_add_const(int a, int b) {
  int c = a + b;
  for (int j = 0; j < 65535; j++) {
      for (int i = 0; i < 64; i++) {
        c += CONSTNUMARR[i];
      }
  }
  return c;
}

// -----------------------------------------------------------------------------

char const A_LONG_STRING[] = "This is a long string which we're storing in the struct";

void test_pointer_manipulation(struct pointer_demo_struct *demo) {
  demo->some_number = sizeof(A_LONG_STRING);
  demo->some_char_ptr = A_LONG_STRING;
}


// -----------------------------------------------------------------------------

typedef void (*fnptr)(void);

fnptr const
__attribute__ ((section (".contsvc_hdr")))
contsvc_fntab[3] = {
    /* 0 */ (fnptr) test_add,
    /* 1 */ (fnptr) test_pointer_manipulation,
    /* 2 */ (fnptr) test_add_const,
};
