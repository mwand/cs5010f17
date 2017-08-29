/* This file should be an executable C program */

/* This will be the type definition for WineOrder, TeaOrder, and
   BarOrder (= WineOrder | TeaOrder) */

#include<stdio.h>
#include<stdlib.h>

typedef char String;

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

typedef struct {
  Size     size;
  TeaType* type;
} *TeaOrder;

/* CONSTRUCTOR FUNCTION */
TeaOrder make_teaorder (Size s,
			 TeaType* t)
{
  TeaOrder o = malloc(sizeof(struct TeaOrder*));
  o->size = s;
  o->type = t;
  return o;
}

/* A BarOrder is one of
   -- a WineOrder
   -- a TeaOrder
 */

/* A BarOrder is represented as a tagged union */

typedef struct {
  enum {W, O} type;
  union {
    WineOrder* wineorder;
    TeaOrder*  teaorder;
  } data;
} *BarOrder;

void tester (BarOrder bo) {
  switch (bo->type) { };
}
	      


void main ()
{
  WineOrder* o = make_wineorder ("Chateau Lafitte", 1993);
  printf("vineyard = %s  year = %s\n",o->vineyard,o->vintage);
  printf("Goodbye, Cruel World.");
}


  
			   

