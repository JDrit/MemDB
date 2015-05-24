#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <arpa/inet.h>
#include <pthread.h>

#include "messages.pb-c.h"

#define handle_error(msg) \
    do { perror(msg); exit(EXIT_FAILURE); } while (0)

char* gen_random(int len) {
    char* s = malloc(len);
    static const char alphanum[] = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
    for (int i = 0 ; i < len ; i++) {
        s[i] = alphanum[rand() % sizeof(alphanum) - 1];
    }
    s[len] = 0;
    return s;
}

void* client_thread(void *args) {
    int sockfd = 0;
    int n = 0;
    uint8_t recvBuff[1024];
    struct sockaddr_in serv_addr;
    memset(recvBuff, '0', sizeof(recvBuff));

    if ((sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
        handle_error("Error: could not create socket");
    }
    serv_addr.sin_family = AF_INET;
    serv_addr.sin_port = htons(5000);
    serv_addr.sin_addr.s_addr = inet_addr("127.0.0.1");
    if (connect(sockfd, (struct sockaddr *)&serv_addr, sizeof(serv_addr)) < 0) {
        handle_error("connect");
    }


    printf("Starting thread\n");
    for (int i = 0 ; i < 5000 ; i++) {
        if (i % 1000 == 0)
            printf("put %d\n", i);
        Messages__ClientRequest request = MESSAGES__CLIENT_REQUEST__INIT;
        Messages__PutRequest put = MESSAGES__PUT_REQUEST__INIT;
        request.type = MESSAGES__TYPE__PUT;
        put.key = gen_random(13);
        put.value = gen_random(15000);
        request.put = &put;
        unsigned len = messages__client_request__get_packed_size(&request);
        void* buf = malloc(len);
        messages__client_request__pack(&request, buf);
        write(sockfd, buf, len);
        n = read(sockfd, recvBuff, sizeof(recvBuff) - 1);
        if (n > 0)
            recvBuff[n] = 0;
        Messages__ClientResponse *response = messages__client_response__unpack(NULL, n, recvBuff);
        if (response != NULL) {
            //printf("put: %d -> %s\n", i, response->put->key);
            messages__client_response__free_unpacked(response, NULL);
        }
        free(buf);
    }
    printf("finishing\n");
    return NULL;
}

int main (int argc, char* argv[]) {
    if (argc == 1) {
        printf("usage: %s <thread count>\n", argv[0]);
        exit(EXIT_FAILURE);
    }
    int threadCount = atoi(argv[1]);
    pthread_t* threads = malloc(sizeof(pthread_t) * threadCount);
    for (int i = 0 ; i < threadCount ; i++) {
        pthread_create(&threads[i], NULL, client_thread, NULL);
    }
    for (int i = 0 ; i < threadCount ; i++) {
        pthread_join(threads[i], NULL);
    }
    return 0;
    /*
    int sockfd = 0;
    int n = 0;
    uint8_t recvBuff[1024];
    char sendBuff[1024];
    struct sockaddr_in serv_addr;
    memset(recvBuff, '0', sizeof(recvBuff));
    memset(sendBuff, '0', sizeof(sendBuff));

    if ((sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
        printf("\n Error: could not create socket");
        return 1;
    }
    serv_addr.sin_family = AF_INET;
    serv_addr.sin_port = htons(5000);
    serv_addr.sin_addr.s_addr = inet_addr("127.0.0.1");
    if (connect(sockfd, (struct sockaddr *)&serv_addr, sizeof(serv_addr)) < 0) {
        handle_error("connect");
        return 1;
    }
    Messages__ClientRequest request = MESSAGES__CLIENT_REQUEST__INIT;
    if (strcmp(argv[1], "get") == 0) {
        request.type = MESSAGES__TYPE__GET;
        Messages__GetRequest get = MESSAGES__GET_REQUEST__INIT;
        get.key = argv[2];
        request.get = &get;
    } else if (strcmp(argv[1], "put") == 0) {
        Messages__PutRequest put = MESSAGES__PUT_REQUEST__INIT;
        request.type = MESSAGES__TYPE__PUT;
        put.key = argv[2];
        put.value = argv[3];
        request.put = &put;
    } else if (strcmp(argv[1], "remove") == 0) {
        Messages__RemoveRequest remove = MESSAGES__REMOVE_REQUEST__INIT;
        request.type = MESSAGES__TYPE__REMOVE;
        remove.key = argv[2];
        request.remove = &remove;
    } else {
        printf("invalid operation %s\n", argv[1]);
        exit(EXIT_FAILURE);
    }
    unsigned len = messages__client_request__get_packed_size(&request);
    void* buf = malloc(len);
    messages__client_request__pack(&request, buf);
    write(sockfd, buf, len);
    n = read(sockfd, recvBuff, sizeof(recvBuff) - 1);
    if (n > 0)
        recvBuff[n] = 0;
    Messages__ClientResponse *response = messages__client_response__unpack(NULL, n, recvBuff);
    if (response != NULL) {
        switch(response->type) {
            case MESSAGES__TYPE__GET:
                printf("%s = %s\n", response->get->key, response->get->value);
                break;
            case MESSAGES__TYPE__PUT:
                printf("put: %s\n", response->put->key);
                break;
            case MESSAGES__TYPE__REMOVE:
                printf("remove: %s\nsuccess: %d", response->remove->key, response->remove->success);
                break;
            default:
                printf("wrong type\n");
        }
    }
    return 0;
    */
}
