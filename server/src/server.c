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
#include "messages.pb-c.h"

#define BACKLOG 10
#define handle_error(msg) \
    do { perror(msg); exit(EXIT_FAILURE); } while (0)


void process_get(Messages__GetResponse *response,
                 Messages__GetRequest *request,
                 DBStore *store) {
    DataValue* value = get_value(store, request->key);
    response->key = malloc(strlen(request->key));
    strcpy(response->key, request->key);
    printf("get key: %s = ", response->key);
    if (value != NULL) {
        response->value = malloc(value->length);
        strncpy(response->value, value->data, value->length);
        printf("%.*s\n", value->length, value->data);
        free_data_value(value);
    } else {
        printf("NOT EXISTS\n");
    }
}

void process_put(Messages__PutResponse *response,
                 Messages__PutRequest *request,
                 DBStore *store) {
    insert_value(store, request->key, strlen(request->value), request->value);
    response->key = malloc(strlen(request->key));
    strcpy(response->key, request->key);
    response->success = true;
}

int main (int argc, char* argv[]) {
    DBStore* store = init_dbstore("test.ind", "test.dat");

    char* key = argv[1];
    if (argc == 2) { // lookup
        printf("key lookup:\n");
        DataValue* value = get_value(store, key);
        if (value != NULL) {
            printf("value=%.*s\n", value->length, value->data);
            free(value->data);
            free(value);
        }
        //write_index(store);
        destroy_dbstore(store);
        return 0;
    } else if (argc ==  3) { // insert
        char* data = argv[2];
        insert_value(store, key, strlen(data), data);
        DataValue* value = get_value(store, key);
        printf("value=%.*s\n", value->length, value->data);
        free_data_value(value);
        //write_index(store);
        destroy_dbstore(store);
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

    if (listen(listenfd, BACKLOG) == -1) {
        printf("Failed to listen\n");
        return -1;
    }
    printf("starting listening...\n");
    while (1) {
        connfd = accept(listenfd, (struct sockaddr*) NULL, NULL);
        int n = read(connfd, recvBuff, sizeof(recvBuff) - 1);
        if (n > 0)
            recvBuff[n] = 0;

        Messages__ClientRequest *request = messages__client_request__unpack(NULL, n, recvBuff);
        Messages__ClientResponse response = MESSAGES__CLIENT_RESPONSE__INIT;
        if (request != NULL) {
            switch (request->type) {
                case MESSAGES__TYPE__GET:
                    printf("get request\n");
                    Messages__GetResponse getResponse = MESSAGES__GET_RESPONSE__INIT;
                    process_get(&getResponse, request->get, store);
                    response.type = MESSAGES__TYPE__GET;
                    response.get = &getResponse;
                    break;
                case MESSAGES__TYPE__PUT:
                    printf("put request\n");
                    Messages__PutResponse putResponse = MESSAGES__PUT_RESPONSE__INIT;
                    process_put(&putResponse, request->put, store);
                    response.type = MESSAGES__TYPE__PUT;
                    response.put = &putResponse;
                    break;
                default:
                    printf("invalid type\n");
            }
            unsigned len = messages__client_response__get_packed_size(&response);
            void *buf = malloc(len);
            messages__client_response__pack(&response, buf);
            write(connfd, buf, len);
        } else {
            printf("failed to parse client request\n");
        }
        close(connfd);
    }

    //write_index(store);
    destroy_dbstore(store);
    return 0;
}
