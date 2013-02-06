#include "int_to_a.h"
#include <stdlib.h>             /* malloc */
#include <assert.h>

struct int_to_a_node {
    uint8_t array_index;
    small_hash__node node;
    int key;
    void *stable_ptr;
};

typedef uint16_t int_to_a__free_bitmap;
#define int_to_a__free_bitmap_ctz  __builtin_ctz

#define INT_TO_A__CHUNK_COUNT   (8*sizeof(int_to_a__free_bitmap))
#define INT_TO_A__CHUNK_FULL_BITMAP ((int_to_a__free_bitmap)~0)
struct int_to_a__chunk {
    int_to_a__free_bitmap free_bitmap;
    struct int_to_a_node nodes[INT_TO_A__CHUNK_COUNT];
    struct list_node usable_chunk_node;
};

static small_hash__hash int_to_a__get_hash(void *user_arg, small_hash__node *node)
{
    return container_of(node, struct int_to_a_node, node)->key;
}

static bool int_to_a__match(void *user_arg, const void *key, small_hash__node *node)
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
    new_chunk->free_bitmap = INT_TO_A__CHUNK_FULL_BITMAP;
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
    assert(chunk->free_bitmap != 0);
    unsigned first_free_index = int_to_a__free_bitmap_ctz(chunk->free_bitmap);
    chunk->free_bitmap &= ~(1ULL << first_free_index);
    if(chunk->free_bitmap == 0) {
        list_del(&chunk->usable_chunk_node);
    }
    struct int_to_a_node *node = &chunk->nodes[first_free_index];
    node->array_index = first_free_index;
    return node;
}

static void free_int_to_a_node(int_to_a__table *table, struct int_to_a_node *node)
{
    struct int_to_a_node *base_nodes = node - node->array_index;
    struct int_to_a__chunk *chunk =
        container_of(base_nodes, struct int_to_a__chunk, nodes);
    assert(!(chunk->free_bitmap & (1ULL << node->array_index)));
    if(0 == chunk->free_bitmap) {
        list_add_before(&table->usable_chunk_list, &chunk->usable_chunk_node);
    }
    chunk->free_bitmap |= (1ULL << node->array_index);
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
