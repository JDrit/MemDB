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
    store->nextSpot = store->dataCapacity; // TODO
    pthread_mutex_init(&store->lock, 0);
    return store;
}

void dbstore_destroy(DBStore* store) {
    index_destroy(store->index);
    check(munmap(store->data, store->dataCapacity) == -1, "data munmap failed");
    check(fsync(store->dataFd) == -1, "data fsync");
    check(close(store->dataFd) == -1, "data close");
    free(store);
}

bool dbstore_remove(DBStore* store, char* key) {
    pthread_mutex_lock(&store->lock);
    bool result = index_remove(store->index, key);
    pthread_mutex_unlock(&store->lock);
    return result;
}

void dbstore_put(DBStore* store, char* key, Messages__Value* value) {
    debug("inserting key %s", key);

    void* data;
    IndexValue dataKey;
    switch (value->value_case) {
        case MESSAGES__VALUE__VALUE_INT_VALUE:
            log_info("put int value");
            dataKey.type = DATA_INT;
            dataKey.length = 4;
            data = &value->intvalue;
            break;
        case MESSAGES__VALUE__VALUE_STRING_VALUE:
            log_info("put string value");
            dataKey.type = DATA_STRING;
            dataKey.length = strlen(value->stringvalue);
            data = value->stringvalue;
            break;
        default:
            log_warn("could not determine data type %d", value->value_case);
            return;
    }
    pthread_mutex_lock(&store->lock);
    if (store->nextSpot + dataKey.length > store->dataCapacity) {
        dbstore_grow(store, dataKey.length);
    }
    // finishes making the index key
    dataKey.offset = store->nextSpot;
    strcpy(dataKey.key, key);

    index_insert(store->index, &dataKey);
    // copies the actual data to the store
    memcpy(store->data + store->nextSpot, data, dataKey.length);
    //check(msync(store->data, store->dataCapacity, MS_SYNC) == -1, "data msync");
    store->nextSpot += dataKey.length;
    pthread_mutex_unlock(&store->lock);
}

Messages__Error dbstore_get(DBStore* store, char* key,
                            Messages__Value* rValue) {
    pthread_mutex_lock(&store->lock);
    DataValue value;
    IndexValue* index = index_get(store->index, key);
    if (index == NULL) {
        pthread_mutex_unlock(&store->lock);
        return MESSAGES__ERROR__NO_VALUE;
    }
    memcpy(value.data, store->data + index->offset, index->length);

    switch (index->type) {
        case MESSAGES__VALUE__VALUE_STRING_VALUE:
            log_info("get string value");
            rValue->value_case = MESSAGES__VALUE__VALUE_STRING_VALUE;
            rValue->stringvalue = value.data;
            break;
        case MESSAGES__VALUE__VALUE_INT_VALUE:
            log_info("get int value");
            rValue->value_case = MESSAGES__VALUE__VALUE_INT_VALUE;
            rValue->intvalue = (int) *value.data;
            break;
        default:
            log_warn("got unknown type %d", index->type);
            return MESSAGES__ERROR__WRONG_VALUE;
    }
    free(index);
    pthread_mutex_unlock(&store->lock);
    return MESSAGES__ERROR__NO_ERROR;
}

/**
 * Grows the data store so that it will have at least length free
 * space. Normally it will double its size every call
 */
static void dbstore_grow(DBStore* store, int length) {
    unsigned long long newsize = store->dataCapacity * 2;
    if (newsize - store->nextSpot < (unsigned long long) length)
        newsize += length;
    debug("Growing to size %lld", newsize);
    check(ftruncate(store->dataFd, newsize) == -1, "store resize ftruncate");
    store->data = mremap(store->data, store->dataCapacity, newsize, MREMAP_MAYMOVE);
    check(store->data == MAP_FAILED, "data resize failed");
    store->dataCapacity = newsize;
}

void data_value_free(DataValue* value) {
    free(value->data);
    free(value);
}
