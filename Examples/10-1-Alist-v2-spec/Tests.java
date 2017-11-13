class Tests {

    public static void main (String[] args) {
        AList<String,Integer> a0 = ALists.empty();

        AList<String,Integer> a1 = a0.extend ("x", 1);
        AList<String,Integer> a2 = a1.extend ("y", 2);
        AList<String,Integer> a3 = a2.extend ("z", 3);
        AList<String,Integer> a4 = a3.extend ("y", 4);

        AList<String,Integer> b2 = a2.extend ("b", 22);
        AList<String,Integer> b3 = b2.extend ("x", 33);
        AList<String,Integer> b4 = b3.extend ("y", 44);

        checkFalse (a0.contains ("x"));
        checkTrue  (a3.contains ("z"));
        checkTrue  (a4.contains ("x"));
        checkFalse (a4.contains ("b"));

        checkTrue  (b2.contains ("b"));
        checkTrue  (b2.contains ("x"));
        checkTrue  (b2.contains ("y"));
        checkFalse (b2.contains ("z"));

        checkTrue (a1.lookup ("x") == 1);
        checkTrue (a2.lookup ("y") == 2);
        checkTrue (a4.lookup ("x") == 1);
        checkTrue (a4.lookup ("y") == 4);
        checkTrue (a4.lookup ("z") == 3);

        checkTrue (b2.lookup ("x") == 1);
        checkTrue (b2.lookup ("y") == 2);
        checkTrue (b4.lookup ("x") == 33);
        checkTrue (b4.lookup ("y") == 44);
        checkTrue (b4.lookup ("b") == 22);

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
            System.err.print(".");
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
