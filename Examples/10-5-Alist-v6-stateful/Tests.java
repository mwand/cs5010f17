class Tests {

    public static void main (String[] args) {
        AList<String,Integer> a0 = ALists.empty();

        a0.extend ("x", 1);
        System.out.println("a0: " + a0.toString());
        a0.extend ("y", 2);
        System.out.println("a0: " + a0.toString());
        a0.extend ("z", 3);
        System.out.println("a0: " + a0.toString());
        a0.extend ("y", 4);
        System.out.println("a0: " + a0.toString());
        a0.extend ("u", 5);
        System.out.println("a0: " + a0.toString());
        a0.extend ("v", 6);
        System.out.println("a0: " + a0.toString());


        checkTrue(a0.contains("x"), "a0 should contain x");
        checkTrue(a0.contains("y"), "a0 should contain y");
        checkTrue(a0.contains("z"), "a0 should contain z");
        checkTrue(a0.contains("u"), "a0 should contain u");
        checkFalse(a0.contains("w"), "a0 should not contain w");        


        // checkTrue(a2.contains("x"), "a2 should contain x");
        // checkTrue(a2.contains("y"), "a2 should contain y");

        // checkTrue(a3.contains("x"), "a3 should contain x");
        // checkTrue(a3.contains("y"), "a3 should contain y");
        // checkTrue(a3.contains("z"), "a3 should contain z");

        // checkTrue(a4.lookup("x")==1, "a4(z) should equal 1");

        // checkTrue(a4.contains("x"), "a4 should contain x (first test)");
        // checkTrue(a4.contains("y"), "a4 ahould contain y");
        // checkTrue(a4.contains("z"), "a4 should contain z");
        // checkFalse(a4.contains("u"), "a4 should contain u");

        // checkTrue(a4.lookup("y")== 4, "a4(y) should equal 4");
        // checkTrue(a4.lookup("z") == 3, "a4(z) should equal 3");

        // System.err.println("end of a4 tests");

        // AList<String,Integer> b2 = a2.extend ("b", 22);
        // AList<String,Integer> b3 = b2.extend ("x", 33);
        // AList<String,Integer> b4 = b3.extend ("y", 44);

        // AList<String,Integer> c0 = ALists.empty();
        // checkFalse (c0.contains ("x"), "c0 should not contain x");
        // AList<String,Integer> c1 = c0.extend("x",11);
        // checkTrue (c1.lookup("x") == 11, "c1(x) should equal 11");
        // AList<String,Integer> c2 = c1.extend("x",22);
        // checkTrue (c1.lookup("x") == 11, "c1(x) should still equal 11");
        // checkTrue (c2.lookup("x") == 22, "c2(x) should equal 22");

        // checkFalse (a0.contains ("x"), "a0 should not contain x");
        // checkTrue  (a3.contains ("z"), "a3 should contain z");
        // checkTrue  (a4.contains ("x"), "a4 should contain x");
        // checkFalse (a4.contains ("b"), "a4 should contain x");

        // checkTrue  (b2.contains ("b"));
        // checkTrue  (b2.contains ("x"));
        // checkTrue  (b2.contains ("y"));
        // checkFalse (b2.contains ("z"));

        // checkTrue (a1.lookup ("x") == 1, "a1(x) should equal 1");
        // checkTrue (a2.lookup ("y") == 2, "a2(x) should equal 2");
        // checkTrue (a4.lookup ("x") == 1, "a4(x) should equal 1");
        // checkTrue (a4.lookup ("y") == 4,  "a4(y) should equal 4");
        // checkTrue (a4.lookup ("z") == 3, "a4(z) should equal 3");

        // checkTrue (b2.lookup ("x") == 1, "b2(x) should equal 1");
        // checkTrue (b2.lookup ("y") == 2,  "b2(y) should equal 2");
        // checkTrue (b4.lookup ("x") == 33, "b4(x) should equal 33");
        // checkTrue (b4.lookup ("y") == 44, "b4(y) should equal 44");
        // checkTrue (b4.lookup ("b") == 22, "b4(b) should equal 22");

        summarize();
    }

    ////////////////////////////////////////////////////////////////

    private static int testsPassed = 0;
    private static int testsFailed = 0;

    private static final String FAILED
        = "    TEST FAILED: ";

    static void checkTrue (boolean result) {
        checkTrue (result, "anonymous");
    }

    static void checkTrue (boolean result, String name) {
        if (result) {
            testsPassed = testsPassed + 1;
            // System.err.print(".");
        }        
        else {
            testsFailed = testsFailed + 1;
            System.err.println (FAILED + name);
        }
    }

    static void checkFalse (boolean result) {
        checkFalse (result, "anonymous");
    }

    static void checkFalse (boolean result, String name) {
        checkTrue (! result, name);
    }

    static void summarize () {
        System.err.println ("Passed " + testsPassed + " tests");
        if (true) {
            System.err.println ("Failed " + testsFailed + " tests");
        }
    }
}
