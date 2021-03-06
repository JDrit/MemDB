#ifndef __dbg_h__
#define __dbg_h__

#include <stdio.h>
#include <errno.h>
#include <string.h>

#ifdef DEBUG

#define debug(M, ...) \
    fprintf(stderr, "[DEBUG]\t (%s:%d):\t" M "\n", __FILE__, __LINE__, ##__VA_ARGS__)

#else

#define debug(M, ...)

#endif

#define clean_errno() (errno == 0 ? "None" : strerror(errno))

#define log_err(M, ...) \
    fprintf(stderr, "[ERROR] (%s:%d: errno: %s) " M "\n", __FILE__, __LINE__, clean_errno(), ##__VA_ARGS__)

#define log_warn(M, ...) \
    fprintf(stderr, "[WARN] (%s:%d: errno: %d %s) " M "\n", __FILE__, __LINE__, errno, clean_errno(), ##__VA_ARGS__)

#define log_info(M, ...) \
    fprintf(stderr, "[INFO]\t (%s:%d):\t" M "\n", __FILE__, __LINE__, ##__VA_ARGS__)

#define check(A, M, ...) \
    if((A)) { \
        log_err(M, ##__VA_ARGS__); \
        exit(EXIT_FAILURE); \
    }

#define sentinel(M, ...) { \
    log_err(M, ##__VA_ARGS__); \
    exit(EXIT_FAILURE); \
}

#define check_mem(A) \
    check((A == NULL), "Out of memory.")

#define check_debug(A, M, ...) \
    if(!(A)) { \
        debug(M, ##__VA_ARGS__); \
        exit(EXIT_FAILURE); \
    }

#endif
