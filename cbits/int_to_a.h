#ifndef __INT_TO_A_H_
#define __INT_TO_A_H_

#include "small_hash.h"

struct int_to_a_node {
    small_hash__node node;
    int key;
    void *stable_ptr;
};

void int_to_a__table__init_dynamic(
    small_hash__table *, unsigned anchors_count);
void int_to_a__table__free(small_hash__table *);

void int_to_a__table__add(small_hash__table *, int key, void *stable_ptr);
void int_to_a__table__del(small_hash__table *, struct int_to_a_node *);
struct int_to_a_node *int_to_a__table__find(small_hash__table *, int key);

void *int_to_a__get_val(struct int_to_a_node *);

#endif
