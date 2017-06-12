// Examples of Data Definition style, in Java

// This file should be an executable Java program

// In Java, most data is internally represented by pointers, as it is
// in Racket.

// In Java, there are no functions, only methods that live in a class.
// The class contains its own constructor method.
// The methods of the class are observers; there may be several
// observers for the same class.



// SCALAR DATA

// Scalar Data, and Itemizations of Scalar Data, can be implemented as
// scalar values.

// Examples:

// A FarenTemp is represented as a Real
// Interp:  r represents the temperature r degrees Farenheit

// A CelsiusTemp is represented as a Real
// Interp:  r represents the temperature r degrees Celsius

// A Size is represented as an int
// WHERE: the size is one of
// -- 8, 12, 16, 20, 30
// INTERP: the size of the cup, in fluid ounces


// A CoffeeType is represented as a String (any string will do)

// A TeaType is represented as a String (any string will do)

// **********************************************************************

// Itemizations of Scalar Data

// These can also be implemented as scalar values

// A MilkType is represented as a String, which must be one of the
// following:
// -- "black"
// -- "skim"
// -- "whole"
// -- "soy"

// CONSTRUCTOR TEMPLATE: not necessary

// OBSERVER EXAMPLE:

class MilkExample {
    static int to_int (String s)
    { switch (s) {
	case "black" : return 1;
	case "skim"  : return 2;
	case "whole" : return 3;
	case "soy"   : return 4;
	default : throw new RunTimeException("illegal MilkType");
	}
    }
}
					     
	

// Information types vs. Data types:

// Java does not have typedefs, so you can't say
// typedef double faren_temp,
// as we did in the C-language examples.

// So we must rely on our contracts to tell us the information that is
// associated with the data  :(

class TempExample {

    // f2c :  FarenTemp -> CelsiusTemp
    // GIVEN: a temperature in Fahrenheit, 
    // RETURNS: the equivalent temperature in Celsius.
    public static double f2c (double f) {
	return ( (5/9) * f - (160 / 9)) ;
	    }

}

// Compound Data

// In Java, compound data is generally represented as a class.
// Instead of a constructor template, we have a constructor
// method.




// ;; A CoffeeOrder is represented as an object of class CoffeeOrderObj
// ;; with the following fields:
// ;; INTERP:
// ;;   size : Size           is the size of cup desired
// ;;   type : CoffeeType     is the kind of coffee order
// ;;   milk : MilkType       is the kind of milk ordered

class CoffeeOrderObj {
    int size;
}
    


class WineOrder {
    String vineyard;
    String vintage ;
    WineOrder (String vy, String vn) {
	vineyard = vy;
	vintage  = vn;
    }
    // no methods yet
}

// TeaOrders:

// A TeaType is represented as a String

// A TeaOrder is represented as an object of class TeaOrderObj with
// interface:

// TeaOrder (Size s, TeaType ty)
// Size to_size ()
// String to_type ()

// class TeaOrderObj {
//     Size size;
//     String type;
//     TeaOrderObj () {};
// }


class DataDefs {

    public static void main (String[] args) {
	SizeObj sz = new SizeObj(20);
	System.out.printf("%s\n",sz.to_String());
	System.out.println("Goodbye, cruel world!");
    }
}











    


