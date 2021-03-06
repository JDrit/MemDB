#ifndef _SERVER_H_
#define _SERVER_H_

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
#include <pthread.h>
#include <signal.h>
#include "dbstore.h"
#include "logging.h"
#include "messages.pb-c.h"

#define BACKLOG 10
#define MAXNTHREAD 10

/**
 * Struct that is passed to all threads to give setup information
 */
struct thread_params {
    int sockfd;
    DBStore* store;
};

/**
 * Processes a get requests and generates a response for it. Takes in points to
 * the request, response, and the database store.
 */
void process_get(Messages__GetResponse *response,
                 Messages__GetRequest *request,
                 DBStore *store);

/**
 * Process a put request and generates the needed response to it. Takes in
 * pointers to the request, response, and database store.
 */
void process_put(Messages__PutResponse *response,
                 Messages__PutRequest *request,
                 DBStore *store);

/**
 * Process a remove request and generates the needed response to it. Take in
 * points to the request, response, and database store.
 */
void process_remove(Messages__RemoveResponse *response,
                    Messages__RemoveRequest *request,
                    DBStore *store);

/**
 * Process a stack pop request and generates the required response. This is
 * where all errors are generated and logged.
 */
void process_push(Messages__PushResponse *response,
                  Messages__PushRequest *request,
                  DBStore *store);

/**
 * Process a stack pop request and generates the required response.
 */
void process_pop(Messages__PopResponse *response,
                 Messages__PopRequest *request,
                 DBStore *store);

void process_enqueue(Messages__EnqueueResponse *response,
                     Messages__EnqueueRequest *request,
                     DBStore *store);

void process__dequeue(Messages__DequeueResponse *response,
                      Messages__DequeueRequest *request,
                      DBStore *store);

void process_peek(Messages__PeekResponse *response,
                  Messages__PeekRequest *request,
                  DBStore *store);

void process_size(Messages__SizeResponse *response,
                  Messages__SizeRequest *request,
                  DBStore *store);

/**
 * The function to run in another thread to handle requests.
 * args is a pointer to a thread_params to give it the needed setup
 * information.
 */
void* connection_thread(void *args);

/**
 * Sets up the needed stuff for accepting connections. Returns a socket
 * that is ready to accept requests. Takes in the port to listen on.
 */
int init_sockfd(int port);

#endif
