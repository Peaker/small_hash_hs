#ifndef __INT_TO_A_INTERNALS_H_
#define __INT_TO_A_INTERNALS_H_

struct int_to_a__table {
    small_hash__table table;
    struct list_node usable_chunk_list; /* Chunk with at least 1 free */
};

#endif
