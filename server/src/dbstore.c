#include "dbstore.h"

#define handle_error(msg) \
    do { perror(msg); exit(EXIT_FAILURE); } while (0)

int dataFd;

DBStore* init_dbstore(char* indexFilename, char* dataFilename) {
    DBStore* store = malloc(sizeof(DBStore));
    store->indexFilename = indexFilename;
    store->dataFilename = dataFilename;
    store->index = g_hash_table_new_full(g_str_hash, g_str_equal, (GDestroyNotify)free_key, (GDestroyNotify)free_value);

    store->nextSpot = read_index(store);

    struct stat sb;
    dataFd = open(store->dataFilename, O_RDWR | O_CREAT , 0644);
    if (dataFd == -1)
        handle_error("data open");
    if (fstat(dataFd, &sb) == -1)
        handle_error("data fstat");
    store->dataCapacity = sb.st_size;
    printf("current capacity: %d\n", store->dataCapacity);
    if (store->dataCapacity == 0) { // increases when file is empty
        ftruncate(open(store->dataFilename, O_RDWR), 1);
        store->dataCapacity = 1;
    }
    store->data = mmap(NULL, store->dataCapacity, PROT_WRITE | PROT_READ, MAP_PRIVATE, dataFd, 0);
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
    fsync(dataFd);
    close(dataFd);
}

void write_index(DBStore* store) {
    GHashTableIter iter;
    gpointer key, value;
    FILE* indexFile = fopen(store->indexFilename, "w");
    g_hash_table_iter_init(&iter, store->index);
    while (g_hash_table_iter_next(&iter, &key, &value)) {
        char* keyName = key;
        IndexValue* dataKey = value;
        fprintf(indexFile, "%s,%u,%u\n", keyName, dataKey->offset, dataKey->length);
    }
    fclose(indexFile);
}

int read_index(DBStore* store) {
    FILE* indexFile = fopen(store->indexFilename, "r");
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
    write(dataFd, store->data, store->dataCapacity);
    store->nextSpot += length;
}

DataValue* get_value(DBStore* store, char* key) {
    DataValue* value = malloc(sizeof(DataValue));
    IndexValue* index = g_hash_table_lookup(store->index, key);
    if (index == NULL)
        return NULL;
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
    if (ftruncate(dataFd, newsize) == -1)
        perror("ftruncate");
    store->data = mremap(store->data, store->dataCapacity, newsize, MREMAP_MAYMOVE);
    if (store->data == MAP_FAILED)
        handle_error("resize fail");
}
