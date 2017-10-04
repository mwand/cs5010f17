// to compile and run this file, say
// javac 05-4-2-interfaces.java
// java Interface_Tests

// a GreenThing is an object of any class that implements the
// GreenThing interface.  

// any class that implements GreenThing must provide foo and bar
// methods with the specified contracts

interface GreenThing {

    int foo ();
    int bar (int n);

}

class C1 implements GreenThing {
    int x;
    int y;
    int r;

    // constructor (usually just annoying boilerplate)
    public C1(int x_init, int y_init, int r_init) {
	x = x_init ; y = y_init; r = r_init; }
    
      
    public int foo () { return x + y; }
    public int bar (int n) { return r + n; }
         
}

class C2 implements GreenThing {
    int a;
    int b;
    int c;

    // constructor (usually just annoying boilerplate)
    public C2(int a_init, int b_init, int c_init) {
	a = a_init; b = b_init; c = c_init; }

    public int foo () { return a + b; }
    public int bar (int n) { return c * n; }

}

class Interface_Tests {

    // this works when o is an object of either a C1 or C2 or any
    // other class that implements GreenThing.
    
    public static int apply_bar (GreenThing o) {
	return o.bar(8);
    }

    public static void main (String[] args) {

	C1 obj1 = new C1(10, 20, 14);
	C1 obj2 = new C1(15, 35, 5)
	C2 obj3 = new C2(15, 35, 5);


	assert obj1.bar(8) == 22;
	assert obj2.bar(8) == 13;
	assert obj3.bar(8) == 40;

	// let's run the same three tests, but using the apply_bar method

	assert apply_bar(obj1) == 22;
	assert apply_bar(obj2) == 13;
	assert apply_bar(obj3) == 40;

	System.out.printf("All tests passed!");
    }
}

	
