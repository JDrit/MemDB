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

#define BACKLOG 10
#define handle_error(msg) \
    do { perror(msg); exit(EXIT_FAILURE); } while (0)


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
    } else if (argc ==  3) { // insert
        char* data = argv[2];
        insert_value(store, key, strlen(data), data);
        DataValue* value = get_value(store, key);
        printf("value=%.*s\n", value->length, value->data);
        free_data_value(value);
    } else {

    for (int i = 0 ; i < 500 ; i++) {
        char* data = argv[2];
        char key[3];
        sprintf(key, "%d", i);
        insert_value(store, key, strlen(data), data);

    }

    for (int i = 0 ; i < 500 ; i++) {
        char key[3];
        sprintf(key, "%d", i);
        DataValue* value = get_value(store, key);
        printf("%s=%.*s\n", key, value->length, value->data);
        free_data_value(value);
    }
    }
    write_index(store);
    destroy_dbstore(store);


    return 0;
    int listenfd = 0;
    int connfd = 0;
    struct sockaddr_in serv_addr;
    char sendBuff[1024];
    char recvBuff[1024];

    listenfd = socket(AF_INET, SOCK_STREAM, 0);
    printf("socket gotten\n");

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

    while (1) {
        connfd = accept(listenfd, (struct sockaddr*) NULL, NULL);
        int n = read(connfd, recvBuff, sizeof(recvBuff) - 1);
        if (n > 0) {
            recvBuff[n] = 0;
            fputs(recvBuff, stdout);
            printf("\n");
        }
        printf("writing\n");
        strcpy(sendBuff, "message message");
        write(connfd, sendBuff, strlen(sendBuff));
        close(connfd);
        sleep(1);
    }
    return 0;
}
