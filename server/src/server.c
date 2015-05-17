#define _GNU_SOURCE 1

#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>

#include "dbstore.h"
#include "logging.h"
#include "messages.pb-c.h"

#define BACKLOG 10
#define handle_error(msg) \
    do { perror(msg); exit(EXIT_FAILURE); } while (0)


void process_get(Messages__GetResponse *response,
                 Messages__GetRequest *request,
                 DBStore *store) {
    DataValue* value = dbstore_get(store, request->key);
    response->key = strdup(request->key);
    if (value != NULL) {
        response->success = true;
        response->value = strndup(value->data, value->length);
        data_value_free(value);
    }
}

void process_put(Messages__PutResponse *response,
                 Messages__PutRequest *request,
                 DBStore *store) {
    dbstore_insert(store, request->key, strlen(request->value), request->value);
    response->key = strdup(request->key);
    response->success = true;
}

int main (int argc, char* argv[]) {
    DBStore* store = dbstore_init("test.ind", "test.dat");

    char* key = argv[1];
    if (argc == 2) { // lookup
        debug("key lookup:\n");
        DataValue* value = dbstore_get(store, key);
        if (value != NULL) {
            debug("value=%.*s\n", value->length, value->data);
            data_value_free(value);
        }
        //write_index(store);
        dbstore_destroy(store);
        return 0;
    } else if (argc ==  3) { // insert
        char* data = argv[2];
        dbstore_insert(store, key, strlen(data), data);
        DataValue* value = dbstore_get(store, key);
        debug("value=%.*s\n", value->length, value->data);
        data_value_free(value);
        dbstore_destroy(store);
        return 0;
    }


    int listenfd = 0;
    int connfd = 0;
    struct sockaddr_in serv_addr;
    void* sendBuff[1024];
    uint8_t recvBuff[1024];

    listenfd = socket(AF_INET, SOCK_STREAM, 0);

    memset(&serv_addr, '0', sizeof(serv_addr));
    memset(sendBuff, '0', sizeof(sendBuff));
    memset(recvBuff, '0', sizeof(recvBuff));

    serv_addr.sin_family = AF_INET;
    serv_addr.sin_addr.s_addr = htonl(INADDR_ANY);
    serv_addr.sin_port = htons(5000);

    bind(listenfd, (struct sockaddr*) &serv_addr, sizeof(serv_addr));

    check(listen(listenfd, BACKLOG) == -1, "failed to lisen to port");
    log_info("starting listening...");
    while (1) {
        connfd = accept(listenfd, (struct sockaddr*) NULL, NULL);
        int n = read(connfd, recvBuff, sizeof(recvBuff) - 1);
        if (n > 0)
            recvBuff[n] = 0;

        Messages__ClientRequest *request = messages__client_request__unpack(NULL, n, recvBuff);
        Messages__ClientResponse response = MESSAGES__CLIENT_RESPONSE__INIT;
        Messages__PutResponse putResponse = MESSAGES__PUT_RESPONSE__INIT;
        Messages__GetResponse getResponse = MESSAGES__GET_RESPONSE__INIT;

        if (request != NULL) {
            switch (request->type) {
                case MESSAGES__TYPE__GET:
                    debug("get request");
                    process_get(&getResponse, request->get, store);
                    response.type = MESSAGES__TYPE__GET;
                    response.get = &getResponse;
                    break;
                case MESSAGES__TYPE__PUT:
                    debug("put request");
                    process_put(&putResponse, request->put, store);
                    response.type = MESSAGES__TYPE__PUT;
                    response.put = &putResponse;
                    break;
                default:
                    log_warn("invalid type");
            }

            unsigned len = messages__client_response__get_packed_size(&response);
            void *buf = malloc(len);
            messages__client_response__pack(&response, buf);
            write(connfd, buf, len);

            // frees up memory
            free(buf);
            switch(request->type) {
                case MESSAGES__TYPE__GET:
                    free(getResponse.key);
                    free(getResponse.value);
                    break;
                case MESSAGES__TYPE__PUT:
                    free(putResponse.key);
                    break;
                default:
                    log_warn("invalid type");
            }
            messages__client_request__free_unpacked(request, NULL);

        } else {
            log_warn("failed to parse client request\n");
        }
        close(connfd);
    }

    dbstore_destroy(store);
    return 0;
}
