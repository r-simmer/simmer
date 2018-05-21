#ifndef simmer__activity_utils_macros_h
#define simmer__activity_utils_macros_h

#define SEP ", "
#define ENDL std::endl
#define BENDL " }" << ENDL

#define LABEL1(a) (#a": ") << a
#define LABEL2(a, b) LABEL1(a) << SEP << LABEL1(b)
#define LABEL3(a, b, c) LABEL1(a) << SEP << LABEL2(b, c)
#define LABEL4(a, b, c, d) LABEL1(a) << SEP << LABEL3(b, c, d)

#define BARE1(a) a
#define BARE2(a, b) BARE1(a) << SEP << BARE1(b)
#define BARE3(a, b, c) BARE1(a) << SEP << BARE2(b, c)
#define BARE4(a, b, c, d) BARE1(a) << SEP << BARE3(b, c, d)

#endif
