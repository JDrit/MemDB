#include "dbindex.h"
#include "xxhash.h"
#include "logging.h"

static DBIndex* index_resize(DBIndex* index);

static unsigned long long determine_filled(DBIndex* index);

static unsigned long long hash_index(DBIndex* index, char* key);

static void index_create(DBIndex* index, unsigned long long size);

static unsigned long long find_slot(DBIndex* index, char* key);

static bool is_occupied(IndexValue* value);

static DBIndex* index_init_new(char* indexFilename, unsigned long long newCap);

/*
 * Creates an index binding with the given size if the file does not exist
 */
static DBIndex* index_init_new(char* indexFilename, unsigned long long newCap) {
    DBIndex *index = malloc(sizeof(DBIndex));
    check_mem(index);
    index->seed = 1;

    struct stat sb;
    index->filename = malloc(1 + strlen(indexFilename));
    check_mem(index->filename);
    strcpy(index->filename, indexFilename);
    index->fd = open(index->filename, O_RDWR | O_CREAT, 0744);
    check(index->fd == -1, "index open failure");
    check(fstat(index->fd, &sb) == -1, "index fstat");
    index->capacity = sb.st_size / sizeof(IndexValue);
    if (index->capacity == 0) {
        index_create(index, newCap); // sets up an index with default values
    } else {
        off_t pa_offset = 0 & ~(sysconf(_SC_PAGE_SIZE) - 1);
        index->index = mmap(NULL, index->capacity * sizeof(IndexValue), PROT_WRITE | PROT_READ,
                MAP_SHARED, index->fd, pa_offset);
        check(index->index == MAP_FAILED, "index mmap");
        index->numFilled = determine_filled(index);
        debug("finished restoring index, capacity: %lld, numFilled: %lld", index->capacity, index->numFilled);
    }

    return index;
}

DBIndex* index_init(char* indexFilename) {
    log_info("initializing index");
    return index_init_new(indexFilename, INITIAL_SIZE);
}

void index_destroy(DBIndex* index) {
    check(munmap(index->index, index->capacity * sizeof(IndexValue)) == -1, "index munmap");
    free(index->filename);
    fsync(index->fd);
    close(index->fd);
    free(index);
    debug("successfully cleaned up index");
}

void index_insert(DBIndex* index, IndexValue* value) {
    debug("inserting %s with offset=%u length=%u", value->key, value->offset, value->length);
    unsigned long long i = find_slot(index, value->key);
    IndexValue curVal;
    memcpy(&curVal, index->index + i * sizeof(IndexValue), sizeof(IndexValue));
    if (is_occupied(&curVal)) { // updates the existing key with a new value
        debug("updating current value");
        memcpy(index->index + i * sizeof(IndexValue), value, sizeof(IndexValue));
        return;
    }
    debug("load factor: %llu", (100 * index->numFilled) / index->capacity);
    if ((100 * index->numFilled) / index->capacity >= LOAD_FACTOR) {
        index_resize(index);
        i = find_slot(index, value->key);
    }
    memcpy(index->index + i * sizeof(IndexValue), value, sizeof(IndexValue));
    check(msync(index->index, index->capacity * sizeof(IndexValue), MS_SYNC) == -1, "index insert msync");
    index->numFilled++;
}

bool index_remove(DBIndex* index, char* key) {
    debug("removing %s", key);
    unsigned long long i = find_slot(index, key);
    IndexValue curVal;
    memcpy(&curVal, index->index + i * sizeof(IndexValue), sizeof(IndexValue));
    if (!is_occupied(&curVal))
        return false;
    unsigned long long j = i;
    while (true) {
        curVal.length = 0;
        curVal.offset = 0;
r2:
        j = (j + 1) % index->capacity;
        memcpy(&curVal, index->index + j * sizeof(IndexValue), sizeof(IndexValue));
        if (!is_occupied(&curVal))
            return true;
        unsigned long long k = hash_index(index, curVal.key);
        if ((i <= j) ? ((i < k) && (k <= j)) : ((i < k) || (k <= j)))
            goto r2;
        memcpy(index->index + i * sizeof(IndexValue), index->index + j * sizeof(IndexValue), sizeof(IndexValue));
        i = j;
    }
    check(msync(index->index, index->capacity * sizeof(IndexValue), MS_SYNC) == -1, "index remove msync");
    index->numFilled--;
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
    debug("getting index for %s", key);
    unsigned long long i = find_slot(index, key);
    IndexValue* value = malloc(sizeof(IndexValue));
    check_mem(value);
    memcpy(value, index->index + i * sizeof(IndexValue), sizeof(IndexValue));
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
static unsigned long long hash_index(DBIndex* index, char* key) {
    unsigned long long hash = XXH64(key, strlen(key), index->seed);
    return hash % (index->capacity);
}

/**
 * Finds the slot in the index where key is stored
 */
static unsigned long long find_slot(DBIndex* index, char* key) {
    unsigned long long i = hash_index(index, key);
    IndexValue value;
    memcpy(&value, index->index + i * sizeof(IndexValue), sizeof(IndexValue));
    while (is_occupied(&value) && strcmp(key, value.key) != 0) {
        i = (i + 1) % index->capacity;
        memcpy(&value, index->index + i * sizeof(IndexValue), sizeof(IndexValue));
    }
    return i;
}

/**
 * Setups the index on disk when the default data. The index is created
 * to the default size and then fill in with elements that all have 0
 * for the offset and length.
 */
static void index_create(DBIndex* index, unsigned long long size) {
    debug("creating index to size %llu\n", size);
    check(ftruncate(index->fd, sizeof(IndexValue) * size) == -1, "index create ftruncate");
    index->capacity = size;
    index->numFilled = 0;
    off_t pa_offset = 0 & ~(sysconf(_SC_PAGE_SIZE) - 1);
    index->index = mmap(NULL, index->capacity * sizeof(IndexValue), PROT_READ | PROT_WRITE,
            MAP_SHARED, index->fd, pa_offset);
    check(index->index == MAP_FAILED, "index mmap");
    check(msync(index->index, index->capacity * sizeof(IndexValue), MS_SYNC) == -1, "index create msync");
    IndexValue value;
    value.offset = 0;
    value.length = 0;
    memset(value.key, '\0', KEY_SIZE); // zeros out the key
    for (unsigned long long i = 0 ; i < size ; i++) {
        memcpy(index->index + i * sizeof(IndexValue), &value, sizeof(IndexValue));
    }
    check(msync(index->index, index->capacity * sizeof(IndexValue), MS_SYNC) == -1, "index create msync");
    debug("finished creating index");
}

static DBIndex* index_resize(DBIndex* index) {
    log_info("increasing index size");
    unsigned long long newCap = index->capacity * 2;
    remove(TMP_FILE);
    DBIndex* newIndex = index_init_new(TMP_FILE, newCap);

    for (unsigned long long i = 0 ; i < index->capacity ; i++) {
        IndexValue value;
        memcpy(&value, index->index + i * sizeof(IndexValue), sizeof(IndexValue));
        if (is_occupied(&value)) {
            debug("copying index %llu", i);
            index_insert(newIndex, &value);
        }
    }
    check(munmap(index->index, index->capacity * sizeof(IndexValue)) == -1, "index munmap");
    rename(TMP_FILE, index->filename);
    index->fd = newIndex->fd;
    index->index = newIndex->index;
    index->index = newIndex->index;
    index->seed = newIndex->seed;
    index->capacity = newIndex->capacity;
    index->numFilled = newIndex->numFilled;
    free(newIndex->filename);
    free(newIndex);
    return newIndex;
}

static unsigned long long determine_filled(DBIndex* index) {
    unsigned long long count = 0L;
    for (unsigned long long i = 0 ; i < index->capacity ; i++) {
        IndexValue value;
        memcpy(&value, index->index + i * sizeof(IndexValue), sizeof(IndexValue));
        if (is_occupied(&value) == true)
            count++;
    }
    return count;
}

/**
 * Determines if the index value is empty of not. Values are determined to be
 * empty if the offset and length are both zero.
 */
static bool is_occupied(IndexValue* value) {
    return value->length != 0;
}

