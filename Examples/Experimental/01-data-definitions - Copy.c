/* Examples of Data Definition style, in C */
/* This file should be an executable C program */

#include<stdio.h>
#include<stdlib.h>

/* In C, definitions must precede uses :( */

/* In C, we will generally represent data other than numerics as
   POINTERS to the underlying data  */



/* SCALAR DATA */

/* A FarenTemp is represented as a Real */
/* Interp:  r represents the temperature r degrees Farenheit */

/* A CelsiusTemp is represented as a Real */
/* Interp:  r represents the temperature r degrees Celsius */

typedef double faren_temp;
typedef double celsius_temp;

/* f2c: FarenTemp -> CelsiusTemp          */
/* GIVEN: a temperature in Fahrenheit,  */
/* RETURNS: the equivalent temperature in Celsius. */

/* celsius_temp f2c (faren_temp f) { ... }  */


/* strings */
typedef char String;

/* **************************************************************** */

/* ;; A Size is represented as one of the following integers: */
/* ;; -- 8, 12, 16, 20, 30 */
/* ;; INTERP: the size of the cup, in fluid ounces */

typedef enum {small = 8, med = 12, large = 16, venti = 20, trenti = 30} Size;

/* NOTE: passing some other number for a Size will not raise an error :P */

/* **************************************************************** */

/* A CoffeeType is represented as a string  (any string will do)  */

typedef String CoffeeType;

/* **************************************************************** */

/* A MilkType is one of */
/* -- black             */
/* -- skim              */
/* -- whole             */
/* -- soy               */

typedef enum {black, skim, whole, soy} MilkType;

/* CONSTRUCTOR PATTERN */

/* MilkType x = black;  (or skim or whole or soy) */

/* OBSERVER PATTERN  */

/* MilkTypeFunction : MilkType -> Sometype */
/*                                         */
/* Sometype MilkTypeFunction (MilkType mt) { */
/*   switch (mt) {                           */
/*   case black : ...; return (...) ;        */
/*   case skim  : ...; return (...) ;        */
/*   case whole : ...; return (...) ;        */
/*   case soy   : ...; return (...) ;        */
/*   }}                                      */

/* **************************************************************** */

/* A CoffeeOrder is represented as a struct containing the
   following fields: */
/* INTERP: */
/*   size : Size           is the size of cup desired */
/*   type : CoffeeType*    is the kind of coffee order */
/*   milk : MilkType       is the kind of milk ordered */

/* We typically work with a _pointer_ to a CoffeeOrder, so we write
   CoffeeOrder*  
*/

typedef struct {
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

/* SomeType CoffeeOrderFn (CoffeeOrder* o) */
/* { sz = o->size; */
/*   ty = o->type; */
/*   m  = o->milk; */
/*   ...; */
/*   return (...): */
/* } */

/* **************************************************************** */
/* WineOrder and its associated types */


/* ;; A Vineyard is represented as a String (any string will do) */

typedef  String Vineyard;

/* CONSTRUCTOR TEMPLATE */

/* Vineyard* y = "..."; */

/* ;; A Vintage  is represented as a positive integer in [1800,2100] */

typedef int Vintage;

/* ********************************************* */

/* A WineOrder is represented as a struct containing the following
   fields: 

   vineyard : Vineyard      the origin of the grapes
   vintage  : Vintage       the vintage of the grape

 */

typedef struct {
  Vineyard* vineyard;
  Vintage   vintage;
} WineOrder;

/* CONSTRUCTOR FUNCTION */

WineOrder* make_wineorder (Vineyard* v,
			   Vintage   y)
{
  WineOrder* w = malloc(sizeof(struct WineOrder*));
  w->vineyard = v;
  w->vintage  = y;
  return w;
}

/* OBSERVER TEMPLATE */

/* WineOrderFn : WineOrder* -> SomeType */

/*
SomeType WineOrderFn (WineOrder* o)
{ v = o->vineyard;
  y = o->vintage;
  ...;
  return ...;                  
}
*/

/* A TeaOrder is represented as a struct containing the following
   fields:

   size : Size      the size of cup desired
   type : TeaType*  the type of tea to be used
   
 */

typedef String TeaType; 	/* any string will do */

typedef struct {
  Size      size;
  TeaType*  type; } TeaOrder;

/* constructor function */

TeaOrder* make_TeaOrder (Size s,
			 TeaType* t)
{
  TeaOrder* o = malloc(sizeof(struct TeaOrder*));
  o->size = s;
  o->type = t;
  return o;
}


/* A BarOrder is one of */
/* -- a CoffeeOrder     */
/* -- a WineOrder       */
/* -- a TeaOrder        */

/* An itemization type that is a union of non-scalars is represented
   as a tagged union */

typedef struct {
  enum {CO_, WO_, TO_} order_type;
  union data_ {
    CoffeeOrder* coffee_order;
    WineOrder*   wine_order;
    TeaOrder*    tea_order;
  } data;
} BarOrder;



/* constructors */

BarOrder* make_coffee_barorder (Size size,
				CoffeeType* type,
				MilkType milk)
{
  BarOrder* o = malloc(sizeof(struct BarOrder*));
  o->order_type = CO_;
  o->data.coffee_order = make_coffeeorder(size, type, milk);
  return o;
}

/* etc. */

/* Observer Template */

/* SomeBarOrderFn : BarOrder* -> SomeType */
/*
SomeType Some BarOrderFn (BarOrder* o)
{
  switch (o->order_type) {
  case CO_ :
    {
      CoffeeOrder* co = o->data.coffee_order;
      Size sz = co->size;
      CoffeeType* ty = co->type;
      MilkType m  = co->milk; 
      ...;
      return ...;
    }
  case WO_ : ...etc...			
  case TO_ : ...etc...
  }
};
*/

/* EXAMPLE:

   Here I've omitted unnecessary components.  Note that for tea
   orders, I could have written

    return o->data.tea_order->size; 

   But long strings of accessors like that are difficult to write
   correctly and to read.  Definitely not beautiful!

 */

/* BarOrder* -> Size */
/* Returns the size of the given bar order. Assumes all wine orders
   are size 'venti' :) */

Size BarOrder_size (BarOrder* o)
{
  switch (o->order_type) {
  case CO_ :
    {
      CoffeeOrder* co = o->data.coffee_order;
      return (co->size);
    }
  case WO_ : 			
    { return(venti); }

  case TO_ :
    {
      TeaOrder* to = o->data.tea_order;
      return (to->size);
      /* return o->data.tea_order->size; */
    }
  }
};

/* **************************************************************** */

/* Lists */

/* An ISBN is represented as an int */

typedef int ISBN;

/* rather than having a struct for a BookStatus and another struct
   for a list of BookStatus, we will write idiomatic C, in which
   these are combined.*/


/* An Inventory is represented as a struct containing the following
   fields: 

   title    : Title*        the title of the first book in the inventory
   isbn     : ISBN          the isbn of the first book in the inventory
   num_on_hand : int        the number of copies of this book on hand
   next     : *Inventory    a pointer to the rest of the inventory, if any.

 */

typedef String Title;

typedef struct inventory_ {
  Title*              title;
  ISBN                isbn;
  int                 num_on_hand;
  struct inventory_ * next;
} Inventory;

/* constructors */

Inventory* empty_inventory = NULL;

Inventory* cons_inventory (Title* t, ISBN isbn, int n, Inventory* inv)
{
  Inventory* o = malloc(sizeof(Inventory*));
  o->title       = t;
  o->isbn        = isbn;
  o->num_on_hand = n;
  o->next        = inv;    
  return o;
}

/* observer template */

/* SomeFunction : Inventory* -> SomeType */

/*
Sometype SomeInvFunction (Inventory* inv)
{
 if (inv == NULL)
   {return ...;}
 else
 {
   String t        = inv->title;
   ISBN i          = inv->isbn;
   int on_hand     = inv->num_on_hand;
   Inventory* rest = inv->next;
   return ...;
 }
}
*/

 
/* example: */
/* total_copies : Inventory* -> int */
/* RETURNS: the total number of books on hand */
int total_copies (Inventory* inv)
{
 if (inv == NULL)
   {return 0;}
 else
 {
   int on_hand     = inv->num_on_hand;
   Inventory* rest = inv->next;
   return (on_hand + total_copies(rest));
 }
}


void main()
{
  CoffeeType* c = "French Roast";
  printf("%s\n", c);
  /* make a coffee order at the bar */
  CoffeeOrder* o = make_coffeeorder(small, c, soy);
  printf("The size is: %d\n", o->size);

  Inventory* i1 = cons_inventory("The Brothers Karamzov", 12345, 101,
				 empty_inventory);
  Inventory* i2 = cons_inventory("War and Peace", 678910, 201, i1);

  printf("Total books = %d\n", total_copies(i2));


  printf("\nGoodbye Cruel World");
}
      
    
  

  
    
