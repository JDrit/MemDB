#ifndef _DBSTORE_H_
#define _DBSTORE_H_

#define _GNU_SOURCE 1

#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <string.h>
#include <pthread.h>
#include "dbindex.h"
#include "messages.pb-c.h"

// used for stack operations
#define PUSHSUCCESS 0
#define NOTINIT 1
#define WRONGTYPE 2
#define EMPTYSTACK 2
#define NOTSTACK 3

typedef struct {
    DBIndex* index;
    char* data;
    unsigned long long nextSpot;
    unsigned long long dataCapacity;
    int dataFd;
    pthread_mutex_t lock;
} DBStore;

// The raw data is read into this struct. This only holds primitive
// data types so both top level data values and node data values
// store individual pieces of data into this store. These structs
// are then mapped into the mmaped data file.
typedef struct {
    char* data;
} DataValue;

DBStore* dbstore_init(char* indexFilename, char* dataFilename);

/**
 * Cleans up the data store and frees all associated memory
 */
void dbstore_destroy(DBStore* dbstore);

/**
 * Gets the value associated with the key. The data type
 * will be filled in to the value argument. This allows the
 * caller knows what type of data it is
 */
Messages__Error dbstore_get(DBStore* dbstore, char* key, Messages__Value* value);

/**
 * Puts a new value at the location, overriding any previous
 * value.
 */
void dbstore_put(DBStore* dbstore, char* key, Messages__Value* value);

/**
 * Removes the value associated with the given key. Returns true
 * of false if the key was in use
 */
bool dbstore_remove(DBStore* dbstore, char* key);

/**
 * Pushes a new value onto a stack at the given key.
 * Returns: an error message to be returned to the client
 */
Messages__Error dbstore_push(DBStore* dbstore, char* key, Messages__Value* value);

/**
 * Pops a new value off of a stack.
 * Returns: an error message to be returned to the client
 */
Messages__Error dbstore_pop(DBStore* dbstore, char* key, Messages__Value* value);

/**
 * Adds a new element to the queue associated with the given key. This will
 * return an error if anything goes wrong
 */
Messages__Error dbstore_enqueue(DBStore* dbstore, char* key, Messages__Value* value);

Messages__Error dbstore_dequeue(DBStore* dbstore, char* key, Messages__Value* value);

Messages__Error dbstore_peek(DBStore* dbstore, char* key, Messages__Value* value);

Messages__Error dbstore_size(DBStore* dbstore, char* key, Messages__Value* value);

/**
 * Frees all the memory used by a DataValue
 */
void data_value_free(DataValue* value);

#endif
