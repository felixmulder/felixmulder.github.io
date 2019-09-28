---
layout: "post"
title: "Re-implementing malloc"
---

This semester at LTH is all about operating systems and implementing effective
C programs. In the operating systems course we mainly discuss Linux, but
emphasize problems applicable to any OS design.

In the OS course project, we implemented the standard C memory functions
`malloc()`, `calloc()` and `realloc()`.

The full implementation is available [here](https://gist.github.com/felixmulder/86e11e3a73297c7f7f90)
and [here](https://gist.github.com/felixmulder/fe13bd81ddcbaf42c216)

The naive approach
------------------

In Unix like operating systems there are two functions that are useful for
manipulating a program's data segment - `sbrk()` and `brk()`.

These functions move the end of the process's data segment, also known as the
*program break*. A simplistic approach to allocate memory would be to let
`malloc()` increase the data segment for each call.

We also know that at some point the user's probably going to want to call
`free()` on the allocated memory. As such, we need to know how many bytes we can
return to the OS. To solve this, we'll allocate a bit more memory than the user
has requested, and prepend a small struct to each chunk of memory we allocate.

```c
typedef struct block_t block_t;
struct block_t {
        int     prepad; /* padding added to align block */
        size_t  size;
        block_t *prev;
        block_t *next;
        char    data[]; /* flexible array member pointing to data */
};

static block_t *available;
```

As you can see, I've added a static pointer to `available`. We will later use
this to keep track of free blocks within our data segment.


```c
void *malloc(size_t size)
{
        if (size < 1)
                return NULL;

        /* find an existing block which satisfies our needs */
        block_t *block = find_available(size);

        if (block == NULL) {
                /*
                 * sbrk() returns the current program break, and increases it by
                 * the argument - here PADDING is the maximum amount of bytes
                 * that we'll need in order to align allocated segment and the
                 * size of block_t
                 */
                char *start = sbrk(size + PADDING);
                int  offset = (uintptr_t)start % sizeof(double);

                block = (void *)(start + offset);

                /* sbrk will return -1 if unsuccessful */
                if (block == (void *)-1)
                        return NULL;

                block->prepad = offset;
                block->size = size;
                block->prev = block;
                block->next = block;
        }

        return block->data;
}
```

When `malloc()` is implemented, we can easily use the implementation to
implement `calloc()` and `realloc()`.

```c
void *calloc(size_t nmemb, size_t size)
{
        size_t total = nmemb * size;
        char   *alloc = malloc(total);

        /* if malloc fails, calloc should fail */
        if (alloc == NULL)
                return NULL;

        memset(alloc, 0, total);
        return alloc;
}

void *realloc(void *ptr, size_t size)
{
        /* realloc can be called with NULL, if so it should work as malloc */
        if (ptr == NULL)
                return malloc(size);

        /* get the block_t containing information about the allocated memory */
        block_t *old_alloc = (void *)((char *)ptr - sizeof(block_t));
        char    *new_alloc = malloc(size);

        if (new_alloc == NULL)
                return NULL;

        for (size_t i = 0; i < old_alloc->size && i < size; i++)
                new_alloc[i] = old_alloc->data[i];

        free(ptr); /* free old memory */
        return new_alloc;
}
```

You should now be able to use the functions described above (if you've redefined
`free()`). The program, however, won't be able to return memory to the system.

Let's fix this by implementing `free()`. By praxis you should allow other calls
to `sbrk()` and `brk()` than from within `malloc()`. This poses a problem to us.
If someone outside of `malloc()` expands the data segment, we won't know about
it. As such, we can only safely shrink the data segment when freeing a block
that ends on the program break.

What do we do with data that isn't on the end? We put it in our linked list
`available`.

```c
void free(void *ptr)
{
        if (ptr == NULL)
                return;

        block_t *block = (void *)((char *)ptr - sizeof(block_t));

        /* a call to sbrk() with 0 as param, yields the program break */
        char *end = sbrk(0);

        if ((char *)ptr + block->size == end) {
                sbrk(-(block->size + PADDING);
        else
                insert_avail(block); /* insert into available */
}
```

But wait, there's more. As aforementioned, we can't decrease the program break
if we're not freeing the last block before the program break. This is where
`available` comes in. Available will keep a list of available memory. If we
find a block big enough, we can use it instead of allocating new memory. Keep
in mind that the block found might be *bigger* than what we need. If we find
such a block, it should be split into two and the piece not being used should
be inserted into the `available` list. Vice versa when we insert into
available, we will merge adjacent blocks.

The realistic implementation
----------------------------

The above implementation works, and is pretty memory efficient. I.e. it uses
only as many resources as it has to, and returns the rest to the OS. It, however,
is not very fast. When increasing the data segment, the kernel gets involved. As
such, getting the memory is actually one of the slowest steps in the above
implementation. Because of this, we want to allocate a large chunk at once, and then
portion it out until it's all used up.

Another shortcoming is the linked list of available blocks. The time to traverse
this list increases linearly. Searching throught the linked list to find adjacent
blocks is an *O(n^2)* operation.

### The Buddy System ###

Most of these shortcomings can be addressed by allocating a big chunk and using
a different structure to save available blocks in. One possible solution is using
the buddy system. The buddy system is very intuitive. This is what we're going
to do:

1. Allocate a big chunk of data with a size the power of two (`1ULL << 32`)
1. When `malloc()` is called, we figure out the nearest power of two (rounding
   up) - we'll call this "the order"
1. We grab the first free block of the nearest order and split it in halves
1. If the halves are of the needed order, we stop. Otherwise, we continue
   splitting the left half into smaller pieces.

This means that each block has a "buddy." This buddy can be found by doing a simple
XOR operation:

```c
/* finding the first block's buddy */
block_t *buddy = pool ^ (1 << block->order);

/* finding the buddy of any block */
block_t *buddy = (void *)(pool + (((char *)block - pool) ^ (1 << block->order)));
```

In the buddy system, we can easily merge available blocks in worst case with a
time of *O(log n)*.

So let's implement `malloc()`. We change the block structure to look like this:

```c
#define MAX_ORDER (26)
#define FREE      (0)
#define RESERVED  (0)

typedef struct block_t block_t;
struct block_t {
        unsigned status : 1; /* RESERVED/FREE */
        size_t order;
        block_t *succ;
        char data[];
};

static block_t *blocks[MAX_ORDER + 1];
static char *pool;
```

This way we can know if the buddy is available for merge. In each
slot of the array we'll save available blocks of the same order.

```c
void *malloc(size_t size)
{
        /* initialize pool */
        if (pool == NULL) {
                char *start = sbrk((1 << MAX_ORDER) + PADDING);

                /* thankfully, we'll only need to align the first block */
                int offset = (uintptr_t)start % sizeof(double);

                block_t *init = blocks[MAX_ORDER] = (void *)(start + offset);
                pool = (void *)blocks[MAX_ORDER];

                init->status = FREE;
                init->order  = MAX_ORDER;
                init->succ   = NULL;
        }

        /* calculate the closest order */
        size_t order = 2;
        while ((1 << order) < (size + sizeof(block_t)))
                order++;

        /* take out the first available block */
        if (blocks[order] != NULL) {
                block_t *block = blocks[order];
                blocks[order] = block->succ;
                block->status = RESERVERD;

                return block->data;
        } else {
                /* split blocks and return first available of correct order */
                return split(order);
        }
}
```

We don't even need to change the implementation of `calloc()` and `realloc()`.
The only thing we need to change is `free()`.

```c
void free(void *ptr)
{
        if (ptr == NULL)
                return;

        block_t *block = (char *)ptr - sizeof(block_t);
        char    *offset = ((char *)block - pool) ^ (1 << block->order);
        block_t *buddy = (void *)(pool + offset);

        /* set current block to free */
        block->status = FREE;

        /* merge with buddies until buddy found not to be free */
        while (block->order < MAX_ORDER &&
               block->status == FREE    &&
               buddy->order == block->order) {
                remove_block(buddy); /* remove from blocks */

                /* some arithmetic to make sure block is leftmost */
                uintptr_t mask = ~(1 << (uintptr_t)block->order);
                uintptr_t ptr = ((char *)block - pool) & mask) + pool;

                block = (block_t *)ptr;
                block->order += 1;

                /* find new buddy! */
                char *offset = ((char *)block - pool) ^ (1 << block->order);
                buddy = (void *)(pool + offset);
        }
}
```

We could've compared the pointers to see which of the blocks were leftmost.
This is, however, much more effective.

Conclusion
----------

Implementing `malloc()` is a fun exercise and gives a look into the difficulties
with memory allocation. Using the buddy system is not fool proof. This method
does not solve the problem of internal fragmentation. If you're interested you
should have a look at [slab allocation](https://en.wikipedia.org/wiki/Slab_allocation).
