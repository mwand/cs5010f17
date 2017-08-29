/* Examples of Data Definition style, in C */
/* This file should be an executable C program */

#include <stdio.h>
#include <stdlib.h>

/* In C, declarations must precede uses :( */

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

celsius_temp f2c (faren_temp f); /* { ... }  */


/* strings */
/* A string is represented by a pointer to an array of characters */
typedef char * String;

/* **************************************************************** */

/* A CoffeeType is represented as a string  (any string will do)  */
typedef String CoffeeType;

/* A Size is a cup size, in ounces */
typedef int Size;


/* **************************************************************** */

/* A CoffeeOrder is represented as a pointer to a struct containing
 *  the following fields:
 * INTERP:
 *   size : Size           the size of the cup being ordered
 *   kind : CoffeeType     the kind of coffee being ordered
*/

typedef struct CoffeeOrder_ {
  Size        size;
  CoffeeType  kind;
  } * CoffeeOrder;

/* CONSTRUCTOR TEMPLATE */

CoffeeOrder make_coffeeorder (Size size,
			      CoffeeType kind)
{
    CoffeeOrder o = malloc(sizeof(o));
    o->size = size;
    o->kind = kind;
    return o;
}

/* An OrderList is a pointer to a singly-linked list of coffee orders. */

typedef struct OrderList_ {
  CoffeeOrder first ;
  struct OrderList_ * rest;
} * OrderList;

OrderList emptylist = NULL;

OrderList cons (CoffeeOrder o, OrderList ol)
{
  OrderList lst = malloc(sizeof(lst));
  lst->first = o;
  lst->rest  = ol;
  return lst;
}

int total_size (OrderList lst)
{
  if (lst == NULL)
    {return 0;}
  else
    {
      int firstsize = lst->first->size;
      int totalrest = total_size(lst->rest);
      return (firstsize + totalrest);
    }
}

int main()
{
  CoffeeType c = "French Roast";
  printf("c = %s\n", c);
  CoffeeOrder o1 = make_coffeeorder(101, c);
  printf("The first size is: %d\n", o1->size);

  CoffeeType c2 = "Decaf";
  CoffeeOrder o2 = make_coffeeorder(201,c2);

  CoffeeOrder o3 = make_coffeeorder(301, "Dunkin Decaf");

  OrderList lst = cons (o3, cons (o2, cons(o1, emptylist)));
  printf("Total of sizes = %d\n", total_size(lst));

  printf("\nGoodbye Cruel World\n");
  return 1;
}
      
    
  

  
    
