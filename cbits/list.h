#ifndef __LIST_H_
#define __LIST_H_

#include <stdbool.h>

struct list_node {
    struct list_node *prev, *next;
};

static inline void list_node_init(struct list_node *);
static inline void list_add_after(struct list_node *where, struct list_node *new);
static inline void list_add_before(struct list_node *where, struct list_node *new);
static inline void list_del(struct list_node *);
static inline bool list_empty(struct list_node *);

static inline void list_node_init(struct list_node *node) {
    node->prev = node->next = node;
}
static inline void list_add_after(struct list_node *point, struct list_node *new) {
    new->next = point->next;
    new->prev = point;
    new->prev->next = new;
    new->next->prev = new;
}
static inline void list_add_before(struct list_node *point, struct list_node *new) {
    new->prev = point->prev;
    new->next = point;
    new->next->prev = new;
    new->prev->next = new;
}
static inline void list_del(struct list_node *new) {
    new->prev->next = new->next;
    new->next->prev = new->prev;
    new->prev = new->next = NULL;
}
static inline bool list_empty(struct list_node *node) {
    return node->prev == node;
}

#endif
