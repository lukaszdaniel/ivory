

#ifndef MINMAX_H_
#define MINMAX_H_

/* Just one global definition of min/max instead of
   many ones spread out across many *.c files */
#ifndef max
#define max(a, b) \
   ({ __typeof__ (a) _a = (a); \
       __typeof__ (b) _b = (b); \
     _a > _b ? _a : _b; })
#endif
#ifndef min
#define min(a, b) \
   ({ __typeof__ (a) _a = (a); \
       __typeof__ (b) _b = (b); \
     _a < _b ? _a : _b; })
#endif

#endif /* MINMAX_H_ */
