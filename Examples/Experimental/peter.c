struct bar_order_ops {
    int (*size)(struct bar_order*);
    /* … */
}
typedef struct bar_order {
    struct bar_order_ops *ops;
    /* public fields - total price? etc. */
    void *private;
}
static int coffee_order_size(struct bar_order* b) {
    struct coffee_order *c = b->private;
    return c->size;
}
static struct bar_order_ops coffee_ops = {
    .size = coffee_order_size;
    /* …. */
}
struct bar_order *make_coffee_order(… arguments) {
    struct bar_order *b = malloc(sizeof(*b));
    struct coffee_order *c = malloc(sizeof(*c));
    c->size = something;
    b->ops = &coffee_ops;
    b->private = c;
    return c;
}
…
struct bar_order *b = make_coffee_order(…);
int size = b->ops.size(b);
