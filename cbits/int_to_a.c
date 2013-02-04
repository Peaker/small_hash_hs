#include "int_to_a.h"
#include <stdlib.h>             /* malloc */
#include <assert.h>

struct int_to_a_node {
    uint8_t array_index;
    small_hash__node node;
    int key;
    void *stable_ptr;
};

union int_to_a__chunk_item {
    struct list_node free_list_node;
    struct int_to_a_node node;
};

#define INT_TO_A__CHUNK_COUNT   16
struct int_to_a__chunk {
    union int_to_a__chunk_item items[INT_TO_A__CHUNK_COUNT];
    struct list_node free_list_head;
    struct list_node usable_chunk_node;
    uint8_t used_count;
};

bool int_to_a__match(void *user_arg, const void *key, small_hash__node *node)
{
    struct int_to_a_node *int_to_a =
        container_of(node, struct int_to_a_node, node);
    return int_to_a->key == (intptr_t)key;
}

static struct small_hash__funcs int_to_a__funcs = SMALL_HASH__FUNCS(int_to_a__);

void int_to_a__table__free(int_to_a__table *table)
{
    /* TODO: Iterate all nodes and free them... */
    small_hash__table__free(&table->table);
}

void int_to_a__table__init_dynamic(
    int_to_a__table *table, unsigned anchors_count)
{
    small_hash__table__init_dynamic(
        &table->table, &int_to_a__funcs, NULL,
        anchors_count);
    list_node_init(&table->usable_chunk_list);
}

static struct int_to_a__chunk *new_chunk(int_to_a__table *table)
{
    struct int_to_a__chunk *new_chunk = malloc(sizeof *new_chunk);
    new_chunk->used_count = 0;
    list_node_init(&new_chunk->free_list_head);
    unsigned i;
    for(i = 0; i < ARRAY_LEN(new_chunk->items); i++) {
        list_add_before(
            &new_chunk->free_list_head, &new_chunk->items[i].free_list_node);
    }
    list_add_after(&table->usable_chunk_list, &new_chunk->usable_chunk_node);
    return new_chunk;
}

static struct int_to_a__chunk *get_usable_chunk(int_to_a__table *table)
{
    if(list_empty(&table->usable_chunk_list)) {
        return new_chunk(table);
    }
    return container_of(
        table->usable_chunk_list.next, struct int_to_a__chunk, usable_chunk_node);
}

static struct int_to_a_node *malloc_from_chunk(struct int_to_a__chunk *chunk)
{
    assert(!list_empty(&chunk->free_list_head));
    struct list_node *first_free = chunk->free_list_head.next;
    list_del(first_free);
    union int_to_a__chunk_item *item =
        container_of(first_free, union int_to_a__chunk_item, free_list_node);
    chunk->used_count++;
    if(chunk->used_count == INT_TO_A__CHUNK_COUNT) {
        list_del(&chunk->usable_chunk_node);
    }
    item->node.array_index = item - chunk->items;
    return &item->node;
}

static void free_to_chunk(
    int_to_a__table *table,
    struct int_to_a__chunk *chunk, union int_to_a__chunk_item *item)
{
    list_add_after(&chunk->free_list_head, &item->free_list_node);
    chunk->used_count++;
    if(1 == chunk->used_count) {
        list_add_before(&table->usable_chunk_list, &chunk->usable_chunk_node);
    }
}

static void free_int_to_a_node(int_to_a__table *table, struct int_to_a_node *node)
{
    union int_to_a__chunk_item *item =
        container_of(node, union int_to_a__chunk_item, node);
    union int_to_a__chunk_item *base_items = item - node->array_index;
    struct int_to_a__chunk *chunk =
        container_of(base_items, struct int_to_a__chunk, items);
    free_to_chunk(table, chunk, item);
}

static struct int_to_a_node *malloc_int_to_a_node(int_to_a__table *table)
{
    struct int_to_a__chunk *chunk = get_usable_chunk(table);
    return malloc_from_chunk(chunk);
}

void int_to_a__table__add(int_to_a__table *table, int key, void *stable_ptr)
{
    struct int_to_a_node *i2a_node = malloc_int_to_a_node(table);
    i2a_node->key = key;
    i2a_node->stable_ptr = stable_ptr;
    small_hash__table__add(&table->table, key, &i2a_node->node);
}

void int_to_a__table__del(int_to_a__table *table, struct int_to_a_node *i2a_node)
{
    small_hash__table__del(&table->table, i2a_node->key, &i2a_node->node);
    free_int_to_a_node(table, i2a_node);
}

struct int_to_a_node *int_to_a__table__find(int_to_a__table *table, int key)
{
    small_hash__node *node =
        small_hash__table__find(&table->table, key, (void*)(intptr_t)key);
    if(!node) return NULL;
    return container_of(node, struct int_to_a_node, node);
}

void *int_to_a__get_val(struct int_to_a_node *node) { return node->stable_ptr; }
