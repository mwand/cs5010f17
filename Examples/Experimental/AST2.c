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
   | a Baz, which is an int

 */

typedef struct {String name; bool flag;} *Bar;

typedef struct {int val;}                *Baz;

typedef struct Foo_ {
  enum {bar, baz} tag;
  union {Bar bar; Baz baz;} data;
} *Foo ;

Bar make_bar(String name, bool flag) {
  Bar o = malloc(sizeof(Bar*));	/* either Foo* or (struct Foo_) works here */
  o->name = name;
  o->flag = flag;
  return o;
}

Baz make_baz(int v) {
  Baz o = malloc(sizeof(Baz*));
  o->val = v;
  return o;
}

Foo Bar2Foo(Bar b) {
  Foo f = malloc(sizeof(Foo*));
  f->tag = bar;
  f->data.bar = bar;
  return f;
}


void main () {
}

    
    
  
