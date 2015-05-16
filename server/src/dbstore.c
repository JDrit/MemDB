#include "dbstore.h"
#include "logging.h"

static void dbstore_grow(DBStore* store, int length);

DBStore* dbstore_init(char* indexFilename, char* dataFilename) {
    log_info("starting dbstore");
    DBStore* store = malloc(sizeof(DBStore));
    check_mem(store);
    store->index = index_init(indexFilename);

    struct stat sb;
    store->dataFd = open(dataFilename, O_RDWR | O_CREAT , 0744);
    check(store->dataFd == -1, "Opening data file");
    check(fstat(store->dataFd, &sb) == -1, "fstat data file");

    store->dataCapacity = sb.st_size;
    if (store->dataCapacity == 0) { // increases when file is empty
        check(ftruncate(store->dataFd, 1) == -1, "data ftruncate");
        store->dataCapacity = 1;
    }
    off_t pa_offset = 0 & ~(sysconf(_SC_PAGE_SIZE) - 1);
    store->dataCapacity -= pa_offset;
    store->data = mmap(NULL, store->dataCapacity, PROT_WRITE | PROT_READ,
            MAP_SHARED, store->dataFd, pa_offset);
    check(store->data == MAP_FAILED, "data mmap failed");
    return store;
}

void dbstore_destroy(DBStore* store) {
    index_destroy(store->index);
    check(munmap(store->data, store->dataCapacity) == -1, "data munmap failed");
    check(fsync(store->dataFd) == -1, "data fsync");
    check(close(store->dataFd) == -1, "data close");
    free(store);
}

void dbstore_insert(DBStore* store, char* key, int length, void* data) {
    debug("inserting key %s", key);
    if (store->nextSpot + length > store->dataCapacity) {
        dbstore_grow(store, length);
    }
    IndexValue* dataKey = malloc(sizeof(IndexValue));
    check_mem(dataKey);
    dataKey->length = length;
    dataKey->offset = store->nextSpot;
    index_insert(store->index, key, dataKey);

    memcpy(store->data + store->nextSpot, data, length);
    check(msync(store->data, store->dataCapacity, MS_SYNC) == -1, "data msync");
    store->nextSpot += length;
}

DataValue* dbstore_get(DBStore* store, char* key) {
    debug("getting data for %s", key);
    IndexValue* index = index_get(store->index, key);
    if (index == NULL) {
        return NULL;
    }
    DataValue* value = malloc(sizeof(DataValue));
    check_mem(value);
    value->length = index->length;
    value->data = malloc(value->length);
    check_mem(value->data);
    printf("using offset: %d\n", index->offset);
    memcpy(value->data, store->data + index->offset, index->length);
    return value;
}

/**
 * Grows the data store so that it will have at least length free
 * space. Normally it will double its size every call
 */
static void dbstore_grow(DBStore* store, int length) {
    long long newsize = store->dataCapacity * 2;
    if (newsize - store->nextSpot < length)
        newsize += length;
    debug("Growing to size %lld", newsize);
    store->dataCapacity = newsize;
    if (ftruncate(store->dataFd, newsize) == -1)
        perror("ftruncate");
    store->data = mremap(store->data, store->dataCapacity, newsize, MREMAP_MAYMOVE);
    check(store->data == MAP_FAILED, "data resize failed");
}

void data_value_free(DataValue* value) {
    free(value->data);
    free(value);
}
