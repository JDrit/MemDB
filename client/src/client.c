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

#include "messages.pb-c.h"

#define handle_error(msg) \
    do { perror(msg); exit(EXIT_FAILURE); } while (0)

int main (int argc, char* argv[]) {
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
    if (argc == 2) {
        printf("key lookup:\n");
        request.type = MESSAGES__TYPE__GET;
        Messages__GetRequest get = MESSAGES__GET_REQUEST__INIT;
        get.key = argv[1];
        request.get = &get;
    } else if (argc == 3) {
        Messages__PutRequest put = MESSAGES__PUT_REQUEST__INIT;
        request.type = MESSAGES__TYPE__PUT;
        put.key = argv[1];
        put.value = argv[2];
        request.put = &put;
    } else {
        printf("wrong number of args\n");
        exit(1);
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
            default:
                printf("wrong type\n");
        }
    }
    return 0;
}
