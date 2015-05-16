#include "dbstore.h"

#define handle_error(msg) \
    do { perror(msg); exit(EXIT_FAILURE); } while (0)

static void dbstore_grow(DBStore* store, int length);

DBStore* dbstore_init(char* indexFilename, char* dataFilename) {
    DBStore* store = malloc(sizeof(DBStore));
    store->index = index_init(indexFilename);

    struct stat sb;
    store->dataFd = open(dataFilename, O_RDWR | O_CREAT , 0744);
    if (store->dataFd == -1)
        handle_error("data open");
    if (fstat(store->dataFd, &sb) == -1)
        handle_error("data fstat");
    store->dataCapacity = sb.st_size;
    if (store->dataCapacity == 0) { // increases when file is empty
        ftruncate(store->dataFd, 1);
        store->dataCapacity = 1;
    }
    off_t pa_offset = 0 & ~(sysconf(_SC_PAGE_SIZE) - 1);
    store->dataCapacity -= pa_offset;
    store->data = mmap(NULL, store->dataCapacity, PROT_WRITE | PROT_READ,
            MAP_SHARED, store->dataFd, pa_offset);
    if (store->data == MAP_FAILED)
        handle_error("data map");
    return store;
}

void dbstore_destroy(DBStore* store) {
    if (store != NULL) {
        index_destroy(store->index);
        if(store->data != NULL) {
            if (munmap(store->data, store->dataCapacity) == -1)
                handle_error("un-map");
        }
        free(store);
    }
    fsync(store->dataFd);
    close(store->dataFd);
}

void dbstore_insert(DBStore* store, char* key, int length, void* data) {
    if (store->nextSpot + length > store->dataCapacity) {
        dbstore_grow(store, length);
    }
    IndexValue* dataKey = malloc(sizeof(IndexValue));
    dataKey->length = length;
    dataKey->offset = store->nextSpot;
    index_insert(store->index, key, dataKey);

    memcpy(store->data + store->nextSpot, data, length);
    if (msync(store->data, store->dataCapacity, MS_SYNC) == -1) {
        handle_error("msync");
    }
    store->nextSpot += length;
}

DataValue* dbstore_get(DBStore* store, char* key) {
    IndexValue* index = index_get(store->index, key);
    if (index == NULL) {
        return NULL;
    }
    DataValue* value = malloc(sizeof(DataValue));
    value->length = index->length;
    value->data = malloc(value->length);
    printf("using offset: %d\n", index->offset);
    memcpy(value->data, store->data + index->offset, index->length);
    return value;
}

/**
 * Grows the data store so that it will have at least length free
 * space. Normally it will double its size every call
 */
static void dbstore_grow(DBStore* store, int length) {
    int newsize = store->dataCapacity * 2;
    if (newsize - store->nextSpot < length)
        newsize += length;
    store->dataCapacity = newsize;
    printf("growing to size: %d\n", newsize);
    if (ftruncate(store->dataFd, newsize) == -1)
        perror("ftruncate");
    store->data = mremap(store->data, store->dataCapacity, newsize, MREMAP_MAYMOVE);
    if (store->data == MAP_FAILED)
        handle_error("resize fail");
}

void data_value_free(DataValue* value) {
    free(value->data);
    free(value);
}
