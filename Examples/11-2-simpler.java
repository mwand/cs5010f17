// simple example of inheritance of methods and fields

interface I {

    // no purpose; these are just for illustration
    int f ();
    int g ();
    int g1 ();
    int h ();
}

class A implements I {
    int i;
    int j;

    A () {
        System.out.println("calling constructor for A");
        this.i = 15;
        this.j = 20;
        System.out.println("i = " + i + " j = " + j);
        System.out.println("");
    }

    public int f () {
        System.out.println("calling method f of class A");
        return (i + j);
    }

    public int g () {
        System.out.println("calling method g of class A");
        return (i + this.f());
    }

    public int g1 () {
        System.out.println("calling method g1 of class A");
        return (i + f());
    }

    public int h () {
        System.out.println("calling method h of class A");
        return (i - j);
    }
}

class B extends A implements I {
    int k;

    B () {
        System.out.println("calling constructor for B");
        k = 200;
        System.out.println("i = " + i + " j = " + j + " k = " + k);
        System.out.println("");
    }

    public int f () {
        System.out.println("calling method f()=this.h() of class B");
        return (this.h());      // which h gets called?
    }

    public int g () {
        System.out.println("calling method g()=super.h() of class B");
        return (super.h());      // which h gets called?
    }

    public int g1 () {
        System.out.println("calling method g1=h() of class B");
        // which h gets called?
        return (h());
    }
    
    public int h () {
        System.out.println("calling method h of class B");
        return (30);
    }



}

class C extends B implements I {

    C () {
        System.out.println("calling constructor for C");
        j = 100;
        System.out.println("i = " + i + " j = " + j + " k = " + k);
        System.out.println("");
        
    }

    public int h () {
        System.out.println("calling method h of class C");
        return (j + k);
    }
}

class Tests {

    public static void main (String[] args) {

        System.out.println("***Starting tests for class A***\n");

        I a1 = new A();

        show("a1.f()", a1.f());
        show("a1.g()", a1.g());
        show("a1.g1()", a1.g1());
        show("a1.h()", a1.h());

        System.out.println("\n***Starting tests for class B***\n");
        
        I b1 = new B();

        show("b1.f()", b1.f());
        show("b1.g()", b1.g());
        show("b1.g1()", b1.g1());
        show("b1.h()", b1.h());

        System.out.println("\n***Starting tests for class C***\n");

        I c1 = new C();

        show("c1.f()", c1.f());
        show("c1.g()", c1.g());
        show("c1.g1()", c1.g1());
        show("c1.h()", c1.h());


    }

    static void show (String str, int n) {
        System.out.println(str + "=" + n + "\n");
    }

}
    
        



            
