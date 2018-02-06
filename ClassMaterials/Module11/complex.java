// more complicated examples of inheritance and fields

interface I {
    int f ();
    int g ();
    int h ();
}

class A implements I {
    int i = 15;
    int j = 20;

    A () {
        System.out.println("calling constructor for A");
        show("i", i); show("j", j);
    }

    // EFFECT: increments i
    // RETURNS: the new value of i + j

    public int f () {
        System.out.println("calling method f of class A");
        i = i + 1;
        return (i + j);
    }

    // RETURNS: mystery!!

    public int g () {
        System.out.println("calling method g of class A");
        return (this.f() + 1);
    }

    public int h () {
        System.out.println("calling method g of class A");
        j = j + 10;
        return (i - j);
    }

    public void show (String str, int n) {
        System.out.println(str + "=" + n);
    }


}

class B extends A implements I {
    int k = 200;

    B () {
        System.out.println("calling constructor for B");
        j = 100;
        int n = this.h();
        show("i", i); show("j", j); show("k", k);
    }

    public int g () {
        System.out.println("calling method g of class B");
        return (i + j + k);
    }

    public int h () {
        System.out.println("calling method h of class B");
        j = j + 100;
        return (super.g() + 30);
    }

}
            
        
class C extends B implements I {

      C () {
        System.out.println("calling constructor for C");  
        show("i", i); show("j", j); show("k", k);
      }

    public int f () {
        System.out.println("calling method f of class C");
        return (j - k);
    }

    public int g () {
        System.out.println("calling method g of class C");
        return (super.h() + 20);
    }
        
      public int h () {
        System.out.println("calling method h of class C");
        k = k + 20;
        return (j + k);
      }
}
class Tests {

    public static void main (String[] args) {

        System.out.println("***Starting tests for class A***");

        I a1 = new A();

        show("a1.f()", a1.f());
        show("a1.g()", a1.g());
        show("a1.h()", a1.h());

        System.out.println("\n***Starting tests for class B***");
        
        I b1 = new B();

        show("b1.f()", b1.f());
        show("b1.g()", b1.g());
        show("b1.h()", b1.h());

        System.out.println("\n***Starting tests for class C***");

        I c1 = new C();

        show("c1.f()", c1.f());
        show("c1.g()", c1.g());
        show("c1.h()", c1.h());


    }

    public static void show (String str, int n) {
        System.out.println(str + "=" + n + "\n");
    }

}
