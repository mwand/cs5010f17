#include<stdio.h>
#include<stdlib.h>
#include<stdbool.h>

typedef char *String;

typedef enum
  {small = 8, med = 12, large = 16, venti = 20, trenti = 30}
  Size;

/* 
   A Foo is one of
   | a Bar, which is a string and a boolean
   | a Baz, which is an int and a ListOf(Foo)

 */

typedef struct Foo_ {
  enum {bar_foo, baz_foo} tag;
  union {
    struct {String name; bool flag;} bar;
    struct {
      int val;
      struct Foo_ *next;
    } baz;
  } data;
} *Foo ;

/* OR: */

/* Have a Baz contain a list of Foos: */

struct Foo2_ {
  enum {bar_foo2, baz_foo2} tag;
  union {
    struct {String name; bool flag;} bar;
    struct {
      int val;
      struct Foo_ next;
    } baz;
  } data;
};
typedef struct Foo2_ Foo2;



Foo make_bar(String name, bool flag) {
  Foo f = malloc(sizeof(Foo*));	/* either Foo* or (struct Foo_) works here */
  f->tag = bar_foo;
  f->data.bar.name = name;
  f->data.bar.flag = flag;
  return f;
}
  
Foo make_baz (int v) {
  Foo f = malloc(sizeof(struct Foo_));
  f->tag = baz_foo;
  f->data.baz.val = v;
  return f;
}

void main () {
	printf("GoodBye Cruel World")
}

    
    
  
