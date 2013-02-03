#include "int_to_a.h"
#include <stdlib.h>             /* malloc */

bool int_to_a__match(void *user_arg, const void *key, small_hash__node *node)
{
    struct int_to_a_node *int_to_a =
        container_of(node, struct int_to_a_node, node);
    return int_to_a->key == (intptr_t)key;
}

static struct small_hash__funcs int_to_a__funcs = SMALL_HASH__FUNCS(int_to_a__);

void int_to_a__table__free(small_hash__table *table)
{
    /* TODO: Iterate all nodes and free them... */
    small_hash__table__free(table);
}

void int_to_a__table__init_dynamic(
    small_hash__table *table, unsigned anchors_count)
{
    small_hash__table__init_dynamic(
        table, &int_to_a__funcs, NULL,
        anchors_count);
}

void int_to_a__table__add(small_hash__table *table, int key, void *stable_ptr)
{
    struct int_to_a_node *i2a_node = malloc(sizeof *i2a_node);
    i2a_node->key = key;
    i2a_node->stable_ptr = stable_ptr;
    small_hash__table__add(table, key, &i2a_node->node);
}

void int_to_a__table__del(small_hash__table *table, struct int_to_a_node *i2a_node)
{
    small_hash__table__del(table, i2a_node->key, &i2a_node->node);
    free(i2a_node);
}

struct int_to_a_node *int_to_a__table__find(small_hash__table *table, int key)
{
    small_hash__node *node =
        small_hash__table__find(table, key, (void*)(intptr_t)key);
    if(!node) return NULL;
    return container_of(node, struct int_to_a_node, node);
}

void *int_to_a__get_val(struct int_to_a_node *node) { return node->stable_ptr; }
