/* This file should be an executable C program */

#include<stdio.h>
#include<stdlib.h>

/* A String is represented as a pointer to a null-terminated array of
   characters. */

typedef char *String;

typedef enum
  {small = 8, med = 12, large = 16, venti = 20, trenti = 30}
  CoffeeSize;

typedef String CoffeeType;

/* CONSTRUCTOR TEMPLATE */

CoffeeType t = "Sumatra"; 

typedef enum {black, skim, whole, soy} MilkType;

MilkType x = black;  

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

/* A CoffeeOrder is represented as a pointer to a struct containing the
   following fields: */
/* INTERP: */
/*   size : Size           is the size of cup desired */
/*   type : CoffeeType*     is the kind of coffee order */
/*   milk : MilkType       is the kind of milk ordered */

typedef struct CoffeeOrder_ {
  CoffeeSize  size;
  CoffeeType  type;
  MilkType    milk; } * CoffeeOrder;
  

/* CONSTRUCTOR FUNCTION */

CoffeeOrder make_coffeeorder (CoffeeSize size,
			      CoffeeType type,
			      MilkType milk)
{
    CoffeeOrder o = malloc(sizeof(struct CoffeeOrder_));
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

/* A TeaType is represented as a string (any string will do) */
typedef String TeaType;

/* A Size is represented as a non-negative integer */
typedef int Size;
/* non-negativeness is not checked */

/* A TeaOrder is a represented as a structure containing the following
   fields: 

   size : Size      -- the size of cup desired, in ounces
   type : TeaType   -- the type of to be used

 */

typedef struct TeaOrder_ {
  Size     size;
  TeaType type;
} *TeaOrder;

/* CONSTRUCTOR FUNCTION */
TeaOrder make_teaorder (Size s,
			TeaType t)
{
  TeaOrder o = malloc(sizeof(struct TeaOrder_));
  o->size = s;
  o->type = t;
  return o;
}

typedef struct {
  enum {order_coffee, order_tea} order_type;
  union {
    TeaOrder* teaorder;
    CoffeeOrder_ coffeeorder;
  } data;
} * Order ;


void tester (Order o) {
  switch (o->order_type) {
  case order_coffee : printf("%s\n", o->data.coffeeorder.type);
  case order_tea    : ;
  }
}


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
  CoffeeType c = "French Roast";
  printf("%s\n", c);
  /* make a coffee order */
  CoffeeOrder o = make_coffeeorder(small, c, soy); 
  printf("%d\n", o->size);
  printf("%s\n", milktype2string(o->milk));
  printf("\nGoodbye Cruel World");
  

}

