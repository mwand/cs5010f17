// In Java, it is often convenient to define static methods
// that deal with some interface type in a separate class
// that does not itself implement the interface.
//
// By convention, the name of that class is the plural of the
// name of the interface.
//
// For example:  Collections is a class that defines static methods
// for the Collection type.

import java.util.Random;

// The UTCs class defines static methods for the UTC type.
// In particular, it defines a static factory method for creating
// values of the UTC type.

public class UTCs {

    // Dynamic isEqual methods should delegate to this static method.

    public static boolean isEqual (UTC t1, UTC t2) {
        return (t1.hour() == t2.hour()) && (t1.minute() == t2.minute());
    }

    // Dynamic hashCode methods should delegate to this static method.

    public static int hashCode (UTC t) {
        return 100 * t.hour() + t.minute();
    }

    // Dynamic toString methods should delegate to this static method.

    public static String toString (UTC t) {
        return ((Integer) (100 * t.hour() + t.minute())).toString();
    }

    // random number generator, to be used as a coin flip

    private static Random coinFlips = new Random();

    // static factory method for creating a UTC
    // GIVEN: the hour in [0,23] and the minute in [0,59]
    // RETURNS: a UTC with that hour and minute

    public static UTC make (int h, int m) {
        // flips a coin to decide whether to return an object
        // of class UTC1 or UTC2

        if (coinFlips.nextBoolean())
            return new UTC1 (h, m);
        else
            return new UTC2 (h, m);
    }

    // main method for testing

    public static void main (String[] args) {
        for (String arg : args)
            System.out.println (arg);
        UTCsTests.main (args);
    }
}

// Tests for UTC objects created by the static factory method in UTCs.

class UTCsTests {

    public static void main (String[] args) {

        // We'll do these tests several times, to increase the
        // probability that objects of different classes will be created.

        int NTESTS = 5;    // how many times we'll run each test

        for (int i = 0; i < NTESTS; i = i + 1) {
            UTC t0000 = UTCs.make (0, 0);      // always test boundary cases
            UTC t0059 = UTCs.make (0, 59);
            UTC t2300 = UTCs.make (23, 0);
            UTC t2359 = UTCs.make (23, 59);
            UTC t1531 = UTCs.make (15, 31);    // and test typical cases

            assert t0000.hour() == 0    : "wrong hour for t0000";
            assert t0000.minute() == 0  : "wrong minute for t0000";
            assert t0059.hour() == 0    : "wrong hour for t0059";
            assert t0059.minute() == 59 : "wrong minute for t0059";
            assert t2300.hour() == 23   : "wrong hour for t2300";
            assert t2300.minute() == 0  : "wrong minute for t2300";
            assert t2359.hour() == 23   : "wrong hour for t2359";
            assert t2359.minute() == 59 : "wrong minute for t2359";
            assert t1531.hour() == 15   : "wrong hour for t1531";
            assert t1531.minute() == 31 : "wrong minute for t1531";

            assert t0000.isEqual(t0000) : "isEqual says t0000 != t0000";
            assert t0059.isEqual(t0059) : "isEqual says t0059 != t0059";
            assert t2300.isEqual(t2300) : "isEqual says t2300 != t2300";
            assert t2359.isEqual(t2359) : "isEqual says t2359 != t2359";
            assert t1531.isEqual(t1531) : "isEqual says t1531 != t1531";

            assert t0000.isEqual(UTCs.make(0, 0))   : "t0000 != t0000";
            assert t0059.isEqual(UTCs.make(0, 59))  : "t0059 != t0059";
            assert t2300.isEqual(UTCs.make(23, 0))  : "t2300 != t2300";
            assert t2359.isEqual(UTCs.make(23, 59)) : "t2359 != t2359";
            assert t1531.isEqual(UTCs.make(15, 31)) : "t1531 != t1531";

            assert ! (t0000.isEqual(t0059)) : "isEqual says t0000 = t0059";
            assert ! (t0059.isEqual(t2359)) : "isEqual says t0059 = t2359";
            assert ! (t2359.isEqual(t2300)) : "isEqual says t2359 = t2300";

            // tests of the usual triumvirate

            assert t0000.equals(t0000) : "equals says t0000 != t0000";
            assert t0059.equals(t0059) : "equals says t0059 != t0059";
            assert t2300.equals(t2300) : "equals says t2300 != t2300";
            assert t2359.equals(t2359) : "equals says t2359 != t2359";
            assert t1531.equals(t1531) : "equals says t1531 != t1531";

            assert t0000.equals(UTCs.make(0, 0))   : "t0000 != t0000";
            assert t0059.equals(UTCs.make(0, 59))  : "t0059 != t0059";
            assert t2300.equals(UTCs.make(23, 0))  : "t2300 != t2300";
            assert t2359.equals(UTCs.make(23, 59)) : "t2359 != t2359";
            assert t1531.equals(UTCs.make(15, 31)) : "t1531 != t1531";

            assert ! (t0000.equals(t0059)) : "equals says t0000 = t0059";
            assert ! (t0059.equals(t2359)) : "equals says t0059 = t2359";
            assert ! (t2359.equals(t2300)) : "equals says t2359 = t2300";

            assert ! (t0000.equals(null))  : "equals says t0000 = null";
            assert ! (t0059.equals("foo")) : "equals says t0059 = a string";

            assert t0000.hashCode() == (UTCs.make(0, 0).hashCode());
            assert t0059.hashCode() == (UTCs.make(0, 59).hashCode());
            assert t2300.hashCode() == (UTCs.make(23, 0).hashCode());
            assert t2359.hashCode() == (UTCs.make(23, 59).hashCode());
            assert t1531.hashCode() == (UTCs.make(15, 31).hashCode());

            assert t0000.toString().equals(UTCs.make(0, 0).toString());
            assert t0059.toString().equals(UTCs.make(0, 59).toString());
            assert t2300.toString().equals(UTCs.make(23, 0).toString());
            assert t2359.toString().equals(UTCs.make(23, 59).toString());
            assert t1531.toString().equals(UTCs.make(15, 31).toString());
        }

        System.out.println ("UTCs tests passed.");
    }
}