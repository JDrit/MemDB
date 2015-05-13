#ifndef _DBSTORE_H_
#define _DBSTORE_H_

#define _GNU_SOURCE 1

#include <glib.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>
#include <string.h>

typedef struct {
    GHashTable* index;
    char* data;
    int nextSpot;
    int dataCapacity;
    char* indexFilename;
    char* dataFilename;
} DBStore;

typedef struct {
    unsigned int offset;
    unsigned int length;
} IndexValue;

typedef struct {
    unsigned int length;
    char* data;
} DataValue;

void free_data_value(DataValue* value);

DBStore* init_dbstore(char* indexFilename, char* dataFilename);

void destroy_dbstore(DBStore* dbstore);

bool does_exist(DBStore* dbstore, char* key);

DataValue* get_value(DBStore* dbstore, char* key);

void insert_value(DBStore* dbstore, char* key, int length, void* data);

bool delete_value(DBStore* dbstore, char* key);

void write_index(DBStore* dbstore);

/**
 * Reads the index from the file and loads in into memory.
 * Returns the current next spot for data
 */
int read_index(DBStore* dbstore);

/**
 * Called by the hash map to free the memory used as the key
 */
void free_key(char* key);

/**
 * Called by the hash map to free the memory used for the value
 */
void free_value(IndexValue* value);

/**
 * Grows the data store so that it will have at least length free
 * space. Normally it will double its size every call
 */
void grow_dbstore(DBStore* store, int length);

#endif
