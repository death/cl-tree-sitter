#include <stdlib.h>
#include <stdbool.h>

#include "api.h"

TSNode *ts_tree_root_node_pointer(const TSTree *self) {
    TSNode *node = malloc(sizeof(TSNode));

    *node = ts_tree_root_node(self);

    return node;
}

TSTreeCursor *ts_tree_cursor_new_pointer(TSNode *node) {
    TSTreeCursor *cursor = malloc(sizeof(TSTreeCursor));

    *cursor = ts_tree_cursor_new(*node);

    return cursor;
}

TSNode *ts_tree_cursor_current_node_pointer(const TSTreeCursor *cursor) {
    TSNode *return_node = malloc(sizeof(TSNode));

    *return_node = ts_tree_cursor_current_node(cursor);

    return return_node;
}

bool ts_node_is_named_pointer(TSNode *node) {
    return ts_node_is_named(*node);
}

TSPoint ts_node_start_point_pointer(TSNode *node) {
    return ts_node_start_point(*node);
}


TSPoint ts_node_end_point_pointer(TSNode *node) {
    return ts_node_end_point(*node);
}

const char *ts_node_type_pointer(TSNode *node) {
    return ts_node_type(*node);
}
