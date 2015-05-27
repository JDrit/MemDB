#define _GNU_SOURCE 1

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
#include <time.h>
#include <stdbool.h>
#include "messages.pb-c.h"

#define handle_error(msg) \
    do { perror(msg); exit(EXIT_FAILURE); } while (0)

pthread_mutex_t m_count;
int count = 0;
clock_t start;

bool sendGet = true;

static char alphanum[] = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";

char* gen_random(int len) {
    char* s = malloc(len);
    for (int i = 0 ; i < len ; i++) {
        s[i] = alphanum[rand() % sizeof(alphanum) - 1];
    }
    s[len - 1] = 0;
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
    serv_addr.sin_port = htons(4000);
    serv_addr.sin_addr.s_addr = inet_addr("127.0.0.1");
    if (connect(sockfd, (struct sockaddr *)&serv_addr, sizeof(serv_addr)) < 0) {
        handle_error("connect");
    }


    printf("Starting thread\n");
    Messages__ClientRequest request = MESSAGES__CLIENT_REQUEST__INIT;
    Messages__PutRequest put = MESSAGES__PUT_REQUEST__INIT;
    Messages__GetRequest get = MESSAGES__GET_REQUEST__INIT;
    if (sendGet)
        request.get = &get;
    else
        request.put = &put;

    for (int i = 0 ; i < 5000000 ; i++) {
        if (sendGet) {
            request.type = MESSAGES__TYPE__GET;
            get.key = "this is a test";
        } else {
            request.type = MESSAGES__TYPE__PUT;
            get.key = "this is a test";
            put.value = alphanum;

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
            messages__client_response__free_unpacked(response, NULL);
            pthread_mutex_lock(&m_count);
            count++;
            if (count % 10000 == 0) {
                if (sendGet)
                    printf("%.2f get requests/sec\n", count / (((double) clock() - start) / CLOCKS_PER_SEC));
                else
                    printf("%.2f put requests/sec\n", count / (((double) clock() - start) / CLOCKS_PER_SEC));
            }
            pthread_mutex_unlock(&m_count);
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
    } else if (argc == 3 && strcmp("put", argv[2]) == 0) {
        sendGet = false;
    }
    int threadCount = atoi(argv[1]);
    pthread_t* threads = malloc(sizeof(pthread_t) * threadCount);
    pthread_mutex_init(&m_count, 0);
    start = clock();
    for (int i = 0 ; i < threadCount ; i++) {
        pthread_create(&threads[i], NULL, client_thread, NULL);
    }
    for (int i = 0 ; i < threadCount ; i++) {
        pthread_join(threads[i], NULL);
    }
    free(threads);
    return 0;
}
