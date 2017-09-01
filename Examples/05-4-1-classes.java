// to compile and run this file, say
// javac 05-4-1-classes.java
// java Class_Tests

class C1 {
    int x;
    int y;
    int r;

    // constructor (usually just annoying boilerplate)
    public C1(int x_init, int y_init, int r_init) {
	x = x_init ; y = y_init; r = r_init; }
    
      
    public int foo () { return x + y; }
    public int bar (int n) { return r + n; }
         
}

class C2 {
    int a;
    int b;
    int c;

    // constructor (usually just annoying boilerplate)
    public C2(int a_init, int b_init, int c_init) {
	a = a_init; b = b_init; c = c_init; }

    public int foo () { return a + b; }
    public int bar (int n) { return c * n; }

}

class Class_Tests {
    public static void main (String[] args) {

	C1 obj1 = new C1(10, 20, 14);
	C1 obj2 = new C1(15, 35, 5);

	assert obj1.bar(8) == 22;
	assert obj2.bar(8) == 13;

	C2 obj3 = new C2(15, 35, 5);

	assert obj3.bar(8) == 40;

	System.out.printf("All tests passed!");
    }
}

	
