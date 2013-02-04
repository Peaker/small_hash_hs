#ifndef __INT_TO_A_H_
#define __INT_TO_A_H_

#include "small_hash.h"
#include "list.h"

struct int_to_a_node;

typedef struct int_to_a__table int_to_a__table;

void int_to_a__table__init_dynamic(
    int_to_a__table *, unsigned anchors_count);
void int_to_a__table__free(int_to_a__table *);

void int_to_a__table__add(int_to_a__table *, int key, void *stable_ptr);
void int_to_a__table__del(int_to_a__table *, struct int_to_a_node *);
struct int_to_a_node *int_to_a__table__find(int_to_a__table *, int key);

void *int_to_a__get_val(struct int_to_a_node *);

#include "int_to_a_internals.h"

#endif
