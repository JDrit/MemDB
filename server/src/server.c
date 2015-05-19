#include "server.h"

pthread_t tid[MAXNTHREAD];
pthread_mutex_t m_acc; // lock used to only accept one request at a time

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

void process_remove(Messages__RemoveResponse *response,
                    Messages__RemoveRequest *request,
                    DBStore *store) {
   response->success = dbstore_remove(store, request->key);
   response->key = strdup(request->key);
}

void* connection_thread(void* args) {
    struct sockaddr_in c_addr;
    socklen_t addrlen;
    int base_sd = ((struct thread_params*) args)->sockfd;
    DBStore* store = ((struct thread_params*) args)->store;
    int sockfd;
    struct timeval timeout;
    timeout.tv_sec = 5;
    timeout.tv_usec = 0;
    while (true) {
        pthread_mutex_lock(&m_acc);
        sockfd = accept(base_sd, &c_addr, &addrlen);
        pthread_mutex_unlock(&m_acc);
        setsockopt(sockfd, SOL_SOCKET, SO_RCVTIMEO, (char *)&timeout, sizeof(timeout));
        setsockopt(sockfd, SOL_SOCKET, SO_SNDTIMEO, (char *)&timeout, sizeof(timeout));
        debug("thread %d accepted socket", (int) pthread_self());

        uint8_t recvBuff[1024];
        memset(recvBuff, '0', sizeof(recvBuff));
        int n = read(sockfd, recvBuff, sizeof(recvBuff) - 1);
        if (n > 0)
            recvBuff[n] = 0;

        Messages__ClientRequest *request = messages__client_request__unpack(NULL, n, recvBuff);
        Messages__ClientResponse response = MESSAGES__CLIENT_RESPONSE__INIT;
        Messages__PutResponse putResponse = MESSAGES__PUT_RESPONSE__INIT;
        Messages__GetResponse getResponse = MESSAGES__GET_RESPONSE__INIT;
        Messages__RemoveResponse removeResponse = MESSAGES__REMOVE_RESPONSE__INIT;

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
                case MESSAGES__TYPE__REMOVE:
                    debug("remove request");
                    process_remove(&removeResponse, request->remove, store);
                    response.type = MESSAGES__TYPE__REMOVE;
                    response.remove = &removeResponse;
                    exit(EXIT_FAILURE);
                    break;
                default:
                    log_warn("invalid type");
            }

            unsigned len = messages__client_response__get_packed_size(&response);
            void *buf = malloc(len);
            messages__client_response__pack(&response, buf);
            if (write(sockfd, buf, len) == -1) {
                log_warn("error while writing to socket");
            }

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
                case MESSAGES__TYPE__REMOVE:
                    free(removeResponse.key);
                    break;
                default:
                    log_warn("invalid type");
            }
            messages__client_request__free_unpacked(request, NULL);

        } else {
            log_warn("failed to parse client request\n");
        }
        close(sockfd);
    }
}

int init_sockfd(int port) {
    struct sockaddr_in in_addr;
    int sockfd;

    bzero(&in_addr, sizeof(struct sockaddr_in));
    in_addr.sin_family = AF_INET;
    in_addr.sin_port = htons(port);
    in_addr.sin_addr.s_addr = INADDR_ANY;

    sockfd = socket(AF_INET, SOCK_STREAM, 0);
    check(bind(sockfd, &in_addr, sizeof(in_addr)) != 0, "bind error");

    check(listen(sockfd, BACKLOG) != 0, "listen error");
    log_info("server listening on port %d", port);
    return sockfd;
}

int main (int argc, char* argv[]) {
    if (argc < 2) {
        printf("usage: %s <port>\n", argv[0]);
        return EXIT_FAILURE;
    }
    DBStore* store = dbstore_init("test.ind", "test.dat");
    int port = atoi(argv[1]);

    struct thread_params* params = malloc(sizeof(struct thread_params));
    params->sockfd = init_sockfd(port);
    params->store = store;
    pthread_mutex_init(&m_acc, 0);
    for (int i = 0 ; i < MAXNTHREAD ; i++)
        pthread_create(&tid[i], 0, connection_thread, (void *) params);
    pause();
    return EXIT_SUCCESS;
}
