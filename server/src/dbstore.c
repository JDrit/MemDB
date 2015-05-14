#include "dbstore.h"

#define handle_error(msg) \
    do { perror(msg); exit(EXIT_FAILURE); } while (0)

DBStore* init_dbstore(char* indexFilename, char* dataFilename) {
    DBStore* store = malloc(sizeof(DBStore));
    store->index = g_hash_table_new_full(g_str_hash, g_str_equal, (GDestroyNotify)free_key, (GDestroyNotify)free_value);

    store->nextSpot = read_index(store, indexFilename);

    struct stat sb;
    store->dataFd = open(dataFilename, O_RDWR | O_CREAT , 0744);
    store->indexFd = open(indexFilename, O_RDWR | O_CREAT | O_APPEND, 0744);
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

void destroy_dbstore(DBStore* store) {
    if (store != NULL) {
        g_hash_table_destroy(store->index);
        if(store->data != NULL) {
            if (munmap(store->data, store->dataCapacity) == -1)
                handle_error("un-map");
        }
        free(store);
    }
    fsync(store->dataFd);
    close(store->dataFd);
}

void write_index_log(DBStore* store, index_log_entry type, char* key, IndexValue* value) {
    // each entry is (type) (index offset) (index length) (key length) key
    char* buf = malloc(sizeof(type) + sizeof(unsigned int) * 2 + sizeof(int) + strlen(key));
    switch (type) {
        case ADD:
            sprintf(buf, "%d%u%u%zd%s", type, value->offset, value->length, strlen(key), key);
            write(store->indexFd, buf, strlen(buf));
            fsync(store->indexFd);
        case DELETE:
            break;
        default:
            printf("unsupported log entry type\n");
    }
}

void write_index(DBStore* store) {
    GHashTableIter iter;
    gpointer key, value;
    FILE* indexFile = fdopen(store->indexFd, "r");
    g_hash_table_iter_init(&iter, store->index);
    while (g_hash_table_iter_next(&iter, &key, &value)) {
        char* keyName = key;
        IndexValue* dataKey = value;
        fprintf(indexFile, "%s,%u,%u\n", keyName, dataKey->offset, dataKey->length);
    }
    fclose(indexFile);
}

int read_index(DBStore* store, char* indexFilename) {
    FILE* indexFile = fopen(indexFilename, "r");
    char line[256];
    int next = 0;
    if (indexFile != NULL) {
        while (fgets(line, sizeof(line), indexFile)) {
            char* sub = strtok(line, ",");
            char* key;
            int length;
            int offset;
            int count = 0;
            while (sub != NULL) {
                switch (count) {
                    case 0:
                        key = sub;
                        break;
                    case 1:
                        offset = atoi(sub);
                        break;
                    case 2:
                        length = atoi(sub);
                        break;
                }
                count++;
                sub = strtok(NULL, ",");
            }
            if (count == 3) {
                IndexValue* dataKey = malloc(sizeof(IndexValue));
                dataKey->length = length;
                dataKey->offset = offset;
                // g_strdup is used since the map needs static strings
                g_hash_table_insert(store->index, (gpointer) g_strdup(key), dataKey);
                if (next < (length + offset)) {
                    next = length + offset;
                }
            }
        }
        fclose(indexFile);
    }
    return next;
}


void insert_value(DBStore* store, char* key, int length, void* data) {
    if (store->nextSpot + length > store->dataCapacity) {
        grow_dbstore(store, length);
    }
    IndexValue* dataKey = malloc(sizeof(IndexValue));
    dataKey->length = length;
    dataKey->offset = store->nextSpot;
    g_hash_table_insert(store->index, (gpointer) g_strdup(key), dataKey);
    memcpy(store->data + store->nextSpot, data, length);
    if (msync(store->data, store->dataCapacity, MS_SYNC) == -1) {
        handle_error("msync");
    }
    write_index_log(store, ADD, key, dataKey);
    store->nextSpot += length;
}

DataValue* get_value(DBStore* store, char* key) {
    IndexValue* index = g_hash_table_lookup(store->index, key);
    if (index == NULL)
        return NULL;
    DataValue* value = malloc(sizeof(DataValue));
    value->length = index->length;
    value->data = malloc(value->length);
    printf("using offset: %d\n", index->offset);
    memcpy(value->data, store->data + index->offset, index->length);
    return value;
}

void free_key(char* key) {
    free(key);
}

void free_value(IndexValue* value) {
    free(value);
}

void free_data_value(DataValue* value) {
    free(value->data);
    free(value);
}

void grow_dbstore(DBStore* store, int length) {
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
