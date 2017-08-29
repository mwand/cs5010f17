/* This file should be an executable C program */

#include<stdio.h>
#include<stdlib.h>

/* In C, definitions must precede uses :( */

/* **************************************************************** */

/* ;; A Size is represented as one of the following integers: */
/* ;; -- 8, 12, 16, 20, 30 */
/* ;; INTERP: the size of the cup, in fluid ounces */

typedef enum {small = 8, med = 12, large = 16, venti = 20, trenti = 30} Size;

/* NOTE: passing some other number for a Size will not raise an error :P */

/* **************************************************************** */

/* ;; A CoffeeType is represented as a string (any string will do) */

/* We typically work with a _pointer_ to a CoffeeType, 
   so we write CoffeeType* */   

typedef char CoffeeType;

typedef char String;
typedef String Foo;

/* CONSTRUCTOR TEMPLATE */

/* CoffeeType* t = "..."; */

/* **************************************************************** */

/* ;; A MilkType is one of */
/* ;; -- "black" */
/* ;; -- "skim"  */
/* ;; -- "whole" */
/* ;; -- "soy"   */

typedef enum {black, skim, whole, soy} MilkType;

/* CONSTRUCTOR PATTERN */

/* MilkType x = black;  (or skim or whole or soy) */

/* OBSERVER PATTERN */

/* MilkTypeFunction : MilkType -> Sometype */
/*                                         */
/* Sometype MilkTypeFunction (MilkType mt) { */
/*   switch (mt) {                           */
/*   case black : ... ; break;               */
/*   case skim  : ... ; break;               */
/*   case whole : ... ; break;               */
/*   case soy   : ... ; break;               */
/*   }}                                      */


/* **************************************************************** */

/* A CoffeeOrder is represented as a struct containing the
   following fields: */
/* INTERP: */
/*   size : Size           is the size of cup desired */
/*   type : CoffeeType*     is the kind of coffee order */
/*   milk : MilkType       is the kind of milk ordered */

/* We typically work with a _pointer_ to a CoffeeOrder, so we write CoffeeOrder* */

typedef struct CoffeeOrder_ {
  Size         size;
  CoffeeType*  type;
  MilkType     milk; } CoffeeOrder;
  

/* CONSTRUCTOR TEMPLATE */

CoffeeOrder* make_coffeeorder (Size size,
			       CoffeeType* type,
			       MilkType milk)
{
    CoffeeOrder* o = malloc(sizeof(struct CoffeeOrder*));
    o->size = size;
    o->type = type;
    o->milk = milk;
    return o;
}

/* OBSERVER TEMPLATE */

/* CoffeeOrderFn : CoffeeOrder* -> SomeType */

/* SomeType CoffeeOrderFn (CoffeeOrder* o)  */
/* { ... o->size ... o->type ... o->milk;   */
/*   return ...;                            */
/* }                                        */

/* **************************************************************** */

/* TESTS */

char* milktype2string (MilkType mt) {
  switch (mt) {
  case black : return ("black");
  case skim  : return ("skim");
  case whole : return ("whole");
  case soy   : return ("soy");
  }}

void main()
{
  CoffeeType* c = "French Roast";
  printf("%s\n", c);
  /* make a coffee order */
  CoffeeOrder* o = make_coffeeorder(small, c, soy); 
  printf("%d\n", o->size);
  printf("%s\n", milktype2string(o->milk));
  printf("\nGoodbye Cruel World");
  

}

