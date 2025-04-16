#ifndef RUNTIME_H
#define RUNTIME_H
val_t entry(val_t*);
extern FILE* in;
extern FILE* out;
extern void (*error_handler)();

// in words
#define heap_size 10000
extern int64_t *heap;
#endif /* RUNTIME_H */
