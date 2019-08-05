#ifndef RT_NETWORK_H
#define RT_NETWORK_H

#include <arpa/inet.h>  /* IP address conversion stuff */
#include <ctype.h>
#include <netdb.h>      /* gethostbyname */
#include <netinet/in.h> /* INET constants and stuff */
#include <string.h>
#include <sys/socket.h> /* socket specific definitions */
#include <sys/types.h>

// Only defined with std=gnu11  or some others
// https://stackoverflow.com/questions/33076175/why-is-struct-addrinfo-defined-only-if-use-xopen2k-is-defined

/* struct addrinfo { */
/*   int              ai_flags; */
/*   int              ai_family; */
/*   int              ai_socktype; */
/*   int              ai_protocol; */
/*   socklen_t        ai_addrlen; */
/*   struct sockaddr *ai_addr; */
/*   char            *ai_canonname; */
/*   struct addrinfo *ai_next; */
/* }; */
struct addrinfo;

int
get_socket_fd (struct addrinfo** return_res);

void
send_udp (int fd, struct addrinfo* res, int c);

void
echo (int sd);

void *
udp_listen ();

void
receive_udp (int fd, struct addrinfo* res);

#endif
