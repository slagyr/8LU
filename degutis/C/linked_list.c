// gcc linked_list.c -o linked_list

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

// library-level stuff

typedef struct _node node;
struct _node {
	int value;
	node* next;
};

node* new_node(int val) {
	node* n = malloc(sizeof(node));
	n->value = val;
	n->next = NULL;
	return n;
}

void walk_nodes(void (*fn)(), node* n) {
	while(n) {
		node* next_node = n->next;
		fn(n);
		n = next_node;
	}
}
void free_node_list(node* n) {
	walk_nodes(free, n);
}

node* last(node* n)
{
  while(n->next)
    n = n->next;
  return n;
}

void append_node(node* head, node* new_tail)
{
  node* tail = last(head);
  tail->next = new_tail;
}

// test-level stuff

static int total;

void total_nodes(node* n) {
	total += n->value;
}

void run_test() {
	total = 0;

	node* head = new_node(42);
	append_node(head, new_node(24));
	append_node(head, new_node(89));
	append_node(head, new_node(11));

	walk_nodes(total_nodes, head);

	free_node_list(head);

	if (total == 166) {
		printf("passed\n");
	}
	else {
		printf("expected total = 166, got %d\n", total);
	}
}




// obligatory-level stuff

int main(int argc, char** argv) {
	run_test();
	return 0;
}
