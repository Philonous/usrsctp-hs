#include <sys/time.h>
#include "usrsctp.h"
#include <stdarg.h>
#include <stdio.h>

void(*log_out)(const char* buffer) = NULL;

void
debug_printf(const char *format, ...) {
    va_list ap;
    char charbuf[1024];
    static struct timeval time_main;
    struct timeval time_now;
    struct timeval time_delta;

            if (time_main.tv_sec == 0  && time_main.tv_usec == 0) {
            gettimeofday(&time_main, NULL);
    }

    gettimeofday(&time_now, NULL);
    timersub(&time_now, &time_main, &time_delta);

    va_start(ap, format);
    if (vsnprintf(charbuf, 1024, format, ap) < 0) {
            charbuf[0] = '\0';
    }
    va_end(ap);

    if (log_out) {
        log_out(charbuf);
    } else
    {
        fprintf(stderr, "[P][%u.%03u] %s", (unsigned int) time_delta.tv_sec
                , (unsigned int) time_delta.tv_usec / 1000, charbuf);
        fflush(stderr);
    }
}

void init_debug( uint16_t port
               , int (*conn_output)(void *addr, void *buffer, size_t length,
                                    uint8_t tos, uint8_t set_df)
               , void (*log_fun)(const char *buffer)
               )
{
    if (log_fun) {
        log_out = log_fun;
        usrsctp_init(port, conn_output, debug_printf);
    } else {
        usrsctp_init(port, conn_output, NULL);
    }
}
