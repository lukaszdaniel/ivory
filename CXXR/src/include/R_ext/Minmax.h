

#ifndef MINMAX_H_
#define MINMAX_H_

/* Just one global definition of min/max instead of
   many ones spread out across many *.c files */
#ifndef max
#define max(a, b) ((a) > (b) ? (a) : (b))
#endif
#ifndef min
#define min(a, b) ((a) < (b) ? (a) : (b))
#endif

#endif /* MINMAX_H_ */
