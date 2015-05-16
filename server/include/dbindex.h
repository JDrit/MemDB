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
#define LOAD_FACTOR 0.8
#define INITIAL_SIZE 1000
#define INDEX_DEBUG 1

typedef struct {
    int fd;
    char* index;             // mmaped data
    unsigned long long seed; // seed used for hashing
    long long capacity;      // the total number of elements possible to be stored
    long long numFilled;     // current number of elements in the index
} DBIndex;

typedef struct {
    unsigned int offset;
    unsigned int length;
    char key[KEY_SIZE];
} IndexValue;

typedef enum {
    ADD,
    DELETE
} index_log_entry_type;

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
void index_insert(DBIndex* index, char* key, IndexValue* value);

bool index_delete(DBIndex* index, char* key);

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
