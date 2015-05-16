#include "dbindex.h"
#include "xxhash.h"
#include "logging.h"

#define handle_error(msg) \
    do { perror(msg); exit(EXIT_FAILURE); } while (0)

static void index_resize(DBIndex* index);

static void read_index(DBIndex* index);

static long long hash_index(DBIndex* index, char* key);

static void index_create(DBIndex* index);

static long long find_slot(DBIndex* index, char* key);

static bool is_occupied(IndexValue* value);

DBIndex* index_init(char* indexFilename) {
    log_info("initializing index");
    DBIndex *index = malloc(sizeof(DBIndex));
    check_mem(index);
    index->seed = 1;

    struct stat sb;
    index->fd = open(indexFilename, O_RDWR | O_CREAT, 0744);
    check(index->fd == -1, "index open failure");
    if (index->fd == -1)
        handle_error("index open");
    if (fstat(index->fd, &sb) == -1)
        handle_error("index fstat");
    index->capacity = sb.st_size / sizeof(IndexValue);
    if (index->capacity == 0) {
        index_create(index); // sets up an index with default values
    } else {
        debug("restoring previous index");
        off_t pa_offset = 0 & ~(sysconf(_SC_PAGE_SIZE) - 1);
        index->index = mmap(NULL, index->capacity * sizeof(IndexValue), PROT_WRITE | PROT_READ,
                MAP_SHARED, index->fd, pa_offset);
        if (index->index == MAP_FAILED)
            handle_error("index mmap");
        index->numFilled = 0; //TODO
        debug("finished restoring index, capacity: %lld, numFilled: %lld\n", index->capacity, index->numFilled);
    }

    return index;

}

void index_destroy(DBIndex* index) {
    if (munmap(index->index, index->capacity * sizeof(IndexValue)) == -1)
        handle_error("index mumap");
    fsync(index->fd);
    close(index->fd);
    free(index);
    debug("successfully cleaned up index");
}

void index_insert(DBIndex* index, char* key, IndexValue* value) {
    debug("inserting %s\n", key);
    long long i = find_slot(index, key);
    IndexValue* curVal = malloc(sizeof(IndexValue));
    check_mem(curVal);
    memcpy(index->index + i * sizeof(IndexValue), curVal, sizeof(IndexValue));
    if (is_occupied(curVal)) { // updates the existing key with a new value
        debug("updating current value");
        memcpy(index->index + i * sizeof(IndexValue), value, sizeof(IndexValue));
        free(curVal);
        return;
    }
    if ((1.0 * index->numFilled) / index->capacity > LOAD_FACTOR) {
        index_resize(index);
    }
    i = find_slot(index, key);
    debug("inserting to slot %lld", i);
    memcpy(index->index + i * sizeof(IndexValue), value, sizeof(IndexValue));
    if (msync(index->index, index->capacity * sizeof(IndexValue), MS_SYNC) == -1)
        handle_error("index insert msync");

}

bool index_remove(DBIndex* index, char* key) {

}

bool index_contains(DBIndex* index, char* key) {
    IndexValue* value = index_get(index, key);
    if (value != NULL) {
        debug("index contains %s", key);
        free(value);
        return true;
    } else {
        debug("index does not contains %s", key);
        return false;
    }
}

IndexValue* index_get(DBIndex* index, char* key) {
    debug("Getting index for %s", key);
    long long i = find_slot(index, key);
    IndexValue* value = malloc(sizeof(IndexValue));
    check_mem(value);
    memcpy(index->index + i * sizeof(IndexValue), value, sizeof(IndexValue));
    if (is_occupied(value) == true) {
        return value;
    } else {
        free(value);
        return NULL;
    }
}

/**
 * Generates the index to be used in the mmaped data
 */
static long long hash_index(DBIndex* index, char* key) {
    long long hash = XXH64(key, sizeof(key), index->seed);
    return hash % (index->capacity);
}

/**
 * Finds the slot in the index where key is stored
 */
static long long find_slot(DBIndex* index, char* key) {
    long long i = hash_index(index, key);
    IndexValue value;
    memcpy(index->index + i, &value, sizeof(IndexValue));
    while (value.offset > 0 && value.length > 0 && strncmp(key, value.key, strlen(key))) {
        i = (i + 1) % index->capacity;
        memcpy(index->index + i, &value, sizeof(IndexValue));
    }
    debug("Slot for %s is %lld", key, i);
    return i;
}

/**
 * Setups the index on disk when the default data. The index is created
 * to the default size and then fill in with elements that all have 0
 * for the offset and length.
 */
static void index_create(DBIndex* index) {
    debug("creating index to size %d\n", INITIAL_SIZE);
    if (ftruncate(index->fd, sizeof(IndexValue) * INITIAL_SIZE) == -1)
        handle_error("index create ftruncate");
    index->capacity = INITIAL_SIZE;
    index->numFilled = 0;
    off_t pa_offset = 0 & ~(sysconf(_SC_PAGE_SIZE) - 1);
    index->index = mmap(NULL, index->capacity * sizeof(IndexValue), PROT_READ | PROT_WRITE,
            MAP_SHARED, index->fd, pa_offset);

    IndexValue value;
    value.offset = 0;
    value.length = 0;
    memset(value.key, '\0', sizeof(value.key)); // zeros out the key
    for (int i = 0 ; i < INITIAL_SIZE ; i++) {
        memcpy(index->index + i * sizeof(IndexValue), &value, sizeof(IndexValue));
    }
    if (msync(index->index, index->capacity * sizeof(IndexValue), MS_SYNC) == -1)
        handle_error("index create msync");
    debug("finished creating index");
}

static void index_resize(DBIndex* index) {
    log_info("increasing index size");

}

/**
 * Determines if the index value is empty of not. Values are determined to be
 * empty if the offset and length are both zero.
 */
static bool is_occupied(IndexValue* value) {
    return value->offset != 0 || value->length != 0;
}

