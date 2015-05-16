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

#include "dbindex.h"

typedef struct {
    DBIndex* index;
    char* data;
    int nextSpot;
    int dataCapacity;
    int dataFd;
} DBStore;

typedef struct {
    unsigned int length;
    char* data;
} DataValue;

DBStore* dbstore_init(char* indexFilename, char* dataFilename);

void dbstore_destroy(DBStore* dbstore);

bool dbstore_contains(DBStore* dbstore, char* key);

DataValue* dbstore_get(DBStore* dbstore, char* key);

void dbstore_insert(DBStore* dbstore, char* key, int length, void* data);

bool delete_value(DBStore* dbstore, char* key);

void data_value_free(DataValue* value);

#endif
