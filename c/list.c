#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <pthread.h>
#include <unistd.h>
#include <stdio.h>
#include <assert.h>

typedef struct node {
  int id;
  char* res;
  struct node * next;
} node_t;

// Push a node to end of list
node_t * push (int id, char* res, node_t *node) {
  while (node->next != NULL) {
    node = node->next;
  }

  node_t next = { id, res, NULL };
  int size = sizeof (node_t);
  node->next = malloc (size);
  memcpy (node->next, &next, size);
  return node;
}

node_t * get_by_id (int id, node_t *node) {
  while (node->id != id && node->next != NULL) {
    node = node->next;
  }
  if (node->id == id) return node;
  return NULL;
}

int main ()
{
  char* res = "First";
  node_t node = { 3, res, NULL };
  push (5, "Second", &node);
  push (333, "All Threes", &node);

  printf ("val: %d // %s\n", node.id, node.res);
  printf ("val: %d // %s\n", node.next->id, node.next->res);
  printf ("val: %d // %s\n", node.next->next->id, node.next->next->res);

  // Try to find
  node_t *node333 = get_by_id (333, &node);
  printf ("val: %d // %s\n", node333->id, node333->res);

  node_t *node5 = get_by_id (5, &node);
  printf ("val: %d // %s\n", node5->id, node5->res);

  printf ("fin.\n");

  exit (0);
}
