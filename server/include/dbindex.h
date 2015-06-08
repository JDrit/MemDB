#ifndef _DBINDEX_H_
#define _DBINDEX_H_

#define _GNU_SOURCE 1

#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/mman.h>

#define KEY_SIZE 15
#define LOAD_FACTOR 80
#define INITIAL_SIZE 10
#define TMP_FILE "tmp.ind"

typedef struct {
    int fd;
    char* filename;
    char* index;                      // mmaped data
    unsigned long long seed;          // seed used for hashing
    unsigned long long capacity;      // the total number of elements possible to be stored
    unsigned long long numFilled;     // current number of elements in the index
} DBIndex;

// all possible top-level data types
typedef enum {
    DATA_INT,
    DATA_STRING,
    DATA_STACK,
    DATA_QUEUE
} data_type;

// types allowed in a node of a stack or queue
typedef enum {
    NODE_INT,
    NODE_STRING,
} node_data_type;

// stores the data for a single node of a stack or queue. next is null when
// this is the end of queue or stack.
typedef struct node {
    node_data_type type;
    unsigned int offset;
    unsigned int length;
    struct node* next;
} node;

// This is the actual struct that is mapped to the on-disk lookup table.
// If the data type is a primitive, then it is stored at the offset, otherwise
// the memory for a node is stored at the offset.
typedef struct {
    data_type type;
    unsigned int offset;
    unsigned int length;
    char key[KEY_SIZE];
} IndexValue;

/*
 * Creates the index and populates all of its variables.
 * Pass it the file name that is used to persistently store all of the index's
 * information
 */
DBIndex* index_init(char *indexFilename);

/*
 * Cleans up all the data used in the index. This includes closing all files
 * and freeing all memory
 */
void index_destroy(DBIndex* index);

/*
 * Inserts the element into the index, overriding any previous value. If the
 * load factor reaches LOAD_FACTOR, then a resize will occur.
 */
void index_insert(DBIndex* index, IndexValue* value);


/*
 * Checks to see if the index exists by calling the index_get function.
 */
bool index_contains(DBIndex* index, char* key);

bool index_remove(DBIndex* index, char* key);

/*
 * Gets the element that corresponds to the given key. Returns the index
 * value if found, else returns NULL.
 */
IndexValue* index_get(DBIndex* index, char* key);

#endif
