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
#include <protobuf-c-rpc/protobuf-c-rpc.h>
#include <protobuf-c-rpc/protobuf-c-rpc-dispatch.h>
#include "messages.pb-c.h"
#include "logging.h"

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

static void handle_response(const Messages__GetResponse *response, void *data) {
    if (response == NULL) {
        log_warn("error processing request");
    } else {
        log_info("response: %s", (char *) response->value);
    }
    *(protobuf_c_boolean *) data = 1;
}

void* client_thread(void *args) {
    int sockfd = 0;
    int n = 0;
    uint8_t recvBuff[1024];
    struct sockaddr_in serv_addr;
    memset(recvBuff, '0', sizeof(recvBuff));

    /*
    if ((sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
        handle_error("Error: could not create socket");
    }
    serv_addr.sin_family = AF_INET;
    serv_addr.sin_port = htons(4000);
    serv_addr.sin_addr.s_addr = inet_addr("127.0.0.1");
    if (connect(sockfd, (struct sockaddr *)&serv_addr, sizeof(serv_addr)) < 0) {
        handle_error("connect");
    }*/


    ProtobufC_RPC_AddressType address_type = PROTOBUF_C_RPC_ADDRESS_TCP;
    ProtobufCService *service = protobuf_c_rpc_client_new(address_type, "localhost:5050", &messages__database__descriptor, NULL);
    ProtobufC_RPC_Client *client = (ProtobufC_RPC_Client *) service;
    //protobuf_c_rpc_client_set_autoreconnect_period (client, 1000);

    log_info("starting to connect...");
    while (!protobuf_c_rpc_client_is_connected(client)) {
    	protobuf_c_rpc_dispatch_run(protobuf_c_rpc_dispatch_default());
        log_info("connecting...");
    }
    log_info("connected");

    Messages__GetRequest request = MESSAGES__GET_REQUEST__INIT;
    request.key = "jd";
    protobuf_c_boolean is_done = 0;
    messages__database__get(service, &request, handle_response, &is_done);
    while (!is_done) {
    	protobuf_c_rpc_dispatch_run(protobuf_c_rpc_dispatch_default());
    }

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
