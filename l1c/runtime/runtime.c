/*
 * EECS 322 Compiler Construction
 * Northwestern University
 *
 * L1 language runtime, including
 * garbage collector functionality.
 *
 * Use the "-m32" GCC flag during compilation!
 *
 * For proper GC behavior, L1 programs 
 * should adhere to the following constraints:
 * 1. immediately before each call to allocate(),
 *    the callee-save registers ebx/edi/esi
 *    should contain either a pointer value, or
 *    a numeric value x ENCODED as 2*x+1 (no
 *    unencoded numeric values!)
 * 2. similarly, immediately before a call to
 *    allocate(), the stack should not contain
 *    unencoded numeric values
 * 3. the boilerplate for the "go" function
 *    should appear exactly as listed in the
 *    lecture notes (that is, exactly 5 things
 *    should be pushed onto the stack before
 *    the body of the function)
 */
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#define HEAP_SIZE 1048576  // one megabyte
//#define HEAP_SIZE 20       // small heap size for testing
#define ENABLE_GC          // uncomment this to enable GC
//#define GC_DEBUG           // uncomment this to enable GC debugging

typedef struct {
   int *allocptr;           // current allocation position
   int words_allocated;
   void **data;
   char *valid;
} heap_t;

heap_t heap;      // the current heap
heap_t heap2;     // the heap for copying

int *stack; // pointer to the bottom of the stack (i.e. value
            // upon program startup)

/*
 * Helper for the print() function
 */
void print_content(void **in, int depth) {
   int i, x, size;
   void **data;

   if(depth >= 4) {
      printf("...");
      return;
   }
   // NOTE: this function crashes quite messily if "in" is 0
   // so we've added this check
   if(in == NULL) {
      printf("nil");
      return;
   }
   x = (int)in;
   if(x & 1) {
      printf("%i", x >> 1);
   } else {
      size= *((int*)in);
      data = in + 1;
      printf("{s:%i", size);
      for(i = 0; i < size; i++) {
         printf(", ");
         print_content(*data, depth + 1);
         data++;
      }
      printf("}");
   }
}

/*
 * Runtime "print" function
 */
int print(void *l) {
   print_content(l, 0);
   printf("\n");

   asm ("movl $1, %%ecx;"
        "movl $1, %%edx;"
      :             // outputs (none)
      :             // inputs (none)
      : "%ecx", "%edx" // clobbered registers (caller-saves, except return val EAX)
   );

   return 1;
}

void reset_heap(heap_t *h) {
   h->allocptr = (int*)h->data;
   h->words_allocated = 0;
}

int alloc_heap(heap_t *h) {
   h->data = (void*)malloc(HEAP_SIZE * sizeof(void*));
   h->valid = (void*)malloc(HEAP_SIZE * sizeof(char));
   reset_heap(h);
   return (h->data != NULL && h->valid != NULL);
}

void switch_heaps() {
   int *temp_allocptr = heap.allocptr;
   int temp_words_allocated = heap.words_allocated;
   void **temp_data = heap.data;
   char *temp_valid = heap.valid;

   heap.allocptr = heap2.allocptr;
   heap.words_allocated = heap2.words_allocated;
   heap.data = heap2.data;
   heap.valid = heap2.valid;

   heap2.allocptr = temp_allocptr;
   heap2.words_allocated = temp_words_allocated;
   heap2.data = temp_data;
   heap2.valid = temp_valid;

   reset_heap(&heap);
}

/*
 * Helper for the gc() function.
 * Copies (compacts) an object from the old heap into
 * the empty heap
 */
int *gc_copy(int *old)  {
   int i, size, array_size;
   int *old_array, *new_array, *first_array_location;
   int valid_index;
   char is_valid;
   char *valid;

   // If not a pointer or not a pointer to a heap location, return input value
   if((int)old % 4 != 0 || (void**)old < heap2.data || (void**)old >= heap2.data + heap2.words_allocated) {
      return old;
   }
   
   // if not pointing at a valid heap object, return input value
   valid_index = (int)((void**)old - heap2.data);
   is_valid = heap2.valid[valid_index];
   if(!is_valid) {
      return old;
   }

   old_array = (int*)old;
   size = old_array[0];
   array_size = size + 1;

   // If the size is negative, the array has already been copied to the
   // new heap, so the first location of array will contain the new address
   if(size == -1) {
       return (int*)old_array[1];
   }
   // If the size is zero, we still have one word of data to copy to the
   // new heap
   else if(size == 0) {
       array_size = 2;
   }

#ifdef GC_DEBUG
   // printf("gc_copy(): valid=%d old=%p new=%p: size=%d asize=%d total=%d\n", is_valid, old, heap.allocptr, size, array_size, heap.words_allocated);
#endif

   valid = heap.valid + heap.words_allocated;

   // Mark the old array as invalid, create the new array
   old_array[0] = -1;
   new_array = heap.allocptr;
   heap.allocptr += array_size;
   heap.words_allocated += array_size;

   // The value of old_array[1] needs to be handled specially
   // since it holds a pointer to the new heap object
   first_array_location = (int*)old_array[1];
   old_array[1] = (int)new_array;

   // Set the values of new_array handling the first two locations separately
   new_array[0] = size;
   new_array[1] = (int)gc_copy(first_array_location);

   valid[0] = 1;
   valid[1] = 0;

   // Call gc_copy on the remaining values of the array
   for (i = 2; i < array_size; i++) {
      new_array[i] = (int)gc_copy((int*)old_array[i]);
      valid[i] = 0;
   }

   return new_array;
}

/*
 * Initiates garbage collection
 */
void gc(int *esp) {
   int i;
   int stack_size = stack - esp + 1;       // calculate the stack size
#ifdef GC_DEBUG
   int prev_words_alloc = heap.words_allocated;

   printf("GC: stack=(%p,%p) (size %d): ", esp, stack, stack_size);
#endif

   // swap in the empty heap to use for storing
   // compacted objects
   switch_heaps();

   // NOTE: the edi/esi register contents could also be
   // roots, but these have been placed in the stack
   // by the allocate() assembly function.  Thus,
   // we only need to look at the stack at this point

   // Then, we need to copy anything pointed at
   // by the stack into our empty heap
   for(i = 0; i < stack_size; i++) {
      esp[i] = (int)gc_copy((int*)esp[i]);
   }

#ifdef GC_DEBUG
   printf("reclaimed %d words\n", (prev_words_alloc - heap.words_allocated));
#endif
}

/*
 * The "allocate" runtime function
 * (assembly stub that calls the 3-argument
 * allocate_helper function)
 */
extern void* allocate(int fw_size, void *fw_fill);
asm(
   ".globl allocate\n"
   ".type allocate, @function\n"
   "allocate:\n"
   "# grab the arguments (into eax,edx)\n"
   "popl %ecx\n" // return val
   "popl %eax\n" // arg 1
   "popl %edx\n" // arg 2
   "# put the original edi/esi on stack instead of args\n"
   "pushl %edi\n" // formerly edx
   "pushl %esi\n" // formerly eax
   "pushl %ebx\n" // formerly return addr  <-- this is the ESP we want
   "pushl %ecx\n" // ecx (return val)
   "pushl %eax\n" // eax (arg 1)
   "pushl %edx\n" // edx (arg 2)
   "# save the original esp (into ecx)\n"
   "movl %esp, %ecx\n"
   "addl $12, %ecx\n"
   "\n"
   "# save the caller's base pointer (so that LEAVE works)\n"
   "# body begins with base and\n"
   "# stack pointers equal\n"
   "pushl %ebp\n"
   "movl %esp, %ebp\n"
   "# push the first three args on stack\n"
   "pushl %ecx\n"
   "pushl %edx\n"
   "pushl %eax\n"
   "# call the real alloc\n"
   "call allocate_helper\n"
   "addl $12, %esp\n"
   "\n"
   "# restore the original base pointer (from stack)\n"
   "leave\n"
   "# restore esi/edi from stack\n"
   "popl %edx\n"  // arg 2
   "popl %ecx\n"  // arg 1
   "addl $4, %esp\n" // skip over return val (it hasn't changed)
   "popl %ebx\n"  // restore ebx
   "popl %esi\n"  // restore esi
   "popl %edi\n"  // restore edi
   "pushl %edx\n" // put back arg 2
   "pushl %ecx\n" // put back arg 1
   "subl $8, %esp\n" // skip over old ebx
   "popl %edx\n"  // original return addr
   "popl %ecx\n"  // junk
   "pushl %edx\n"  // restore return addr
   "movl $1, %ecx\n" // make sure the caller-saves don't have fake-ptr garbage
   "movl $1, %edx\n"
   "ret\n" 
);

/*
 * The real "allocate" runtime function
 * (called by the above assembly stub function)
 */
void* allocate_helper(int fw_size, void *fw_fill, int *esp)
{
   int i, data_size, array_size;
   char *valid;
   int *ret;

   if(!(fw_size & 1)) {
      printf("allocate called with size input that was not an encoded integer, %i\n",
             fw_size);
      exit(-1);
   }

   data_size = fw_size >> 1;

   if(data_size < 0) {
      printf("allocate called with size of %i\n", data_size);
      exit(-1);
   }

#ifdef GC_DEBUG
   //printf("runtime.c: allocate(%d,%d (%p)) @ %p: ESP = %p (%d), EDI = %p (%d), ESI = %p (%d), EBX = %p (%d)\n",
   //       data_size, (int)fw_fill, fw_fill, heap.allocptr, esp, (int)esp, (int*)esp[2], esp[2],
   //       (int*)esp[1], esp[1], (int*)esp[0], esp[0]);
   //fflush(stdout);
#endif

   // Even if there is no data, allocate an array of two words
   // so we can hold a forwarding pointer and an int representing if
   // the array has already been garbage collected
   array_size = (data_size == 0) ? 2 : data_size + 1;

   // Check if the heap has space for the allocation
   if(heap.words_allocated + array_size >= HEAP_SIZE)
   {
#ifdef ENABLE_GC
      // Garbage collect
      gc(esp);

      // Check if the garbage collection free enough space for the allocation
      if(heap.words_allocated + array_size >= HEAP_SIZE) {
#endif
         printf("out of memory\n"); // NOTE: we've added a newline
         exit(-1);
#ifdef ENABLE_GC
      }
#endif
   }

   // Do the allocation
   ret = heap.allocptr;
   valid = heap.valid + heap.words_allocated;
   heap.allocptr += array_size;
   heap.words_allocated += array_size;

   // Set the size of the array to be the desired size
   ret[0] = data_size;

   // record this as a heap object
   valid[0] = 1;

   // If there is no data, set the value of the array to be a number
   // so it can be properly garbage collected
   if(data_size == 0) {
      ret[1] = 1;
      valid[1] = 0;
   } else {
      // Fill the array with the fill value
      for(i = 1; i < array_size; i++) {
         ret[i] = (int)fw_fill;
         valid[i] = 0;
      }
   }

   return ret;
}

/*
 * The "array-error" runtime function
 */
int array_error(int *array, int fw_x) {
   printf("attempted to use position %i in an array that only has %i positions\n",
          fw_x >> 1, *array);
   exit(0);
}

/*
 * Program entry-point
 */
int main() {
   int b1 = alloc_heap(&heap);
   int b2 = alloc_heap(&heap2);

   if(!b1 || !b2) {
      printf("malloc failed\n");
      exit(-1);
   }

   // Move esp into the bottom-of-stack pointer.
   // The "go" function's boilerplate (as long as one copies it
   // correctly from the lecture notes), in conjunction with
   // the C calling convention dictates that there will be
   // exactly 6 words added to the stack before the
   // body of "go" actually happens
   asm ("movl %%esp, %%eax;"
        "subl $24, %%eax;" // 6 * 4
        "movl %%eax, %0;"
        "movl $1, %%eax;"  // clear the caller-saves
        "movl $1, %%ecx;"
        "movl $1, %%edx;"
        "movl $1, %%ebx;"  // clear the callee-saves
        "movl $1, %%edi;"
        "movl $1, %%esi;"
        "call go;"
      : "=m"(stack) // outputs
      :             // inputs (none)
      : "%eax", "%ecx", "%edx", "%ebx", "%edi", "%esi" // clobbered registers (caller- & callee-saves)
   );  

#ifdef GC_DEBUG
   printf("runtime.c: main(): initial ESP value = %p (%d)\n", stack, (int)stack);
#endif

   return 0;
}
