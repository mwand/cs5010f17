// Constructor template for UTC1:
//     new UTC1 (h, m)
// Interpretation:
//     h is the hour (between 0 and 23, inclusive)
//     m is the minute (between 0 and 59, inclusive)

class UTC1 extends UTC0 implements UTC {

    int h;    // the hour, limited to [0,23]
    int m;    // the minute, limited to [0,59]

    // the Java constructor

    UTC1 (int h, int m) {
        this.h = h;
        this.m = m;
    }

    // public methods

    // Returns the hour, between 0 and 23 inclusive.

    public int hour () { return h; }

    // Returns the minute, between 0 and 59 inclusive.

    public int minute () { return m; }

    // The isEqual, equals, hashCode, and toString
    // methods are inherited from UTC0.

    // a main method for unit testing

    public static void main (String[] args) {
        UTC1tests.main(args);
    }
}

// Unit tests for UTC1.

class UTC1tests {

    public static void main (String[] args) {
        UTC t1 = new UTC1 (15, 31);
        UTC t2 = new UTC1 (14, 31);
        UTC t3 = new UTC1 (15, 32);
        UTC t4 = new UTC1 (15, 31);

        assert t1.hour() == 15 : "wrong hour for t1";
        assert t1.minute() == 31 : "wrong minute for t1";

        assert t1.isEqual (t1) : "isEqual says this doesn't equal this";
        assert t1.isEqual (t4) : "isEqual says this doesn't equal that";
        assert ! (t1.isEqual (t2)) : "isEqual true but hour different";
        assert ! (t1.isEqual (t3)) : "isEqual true but minute different";

        System.out.println ("All unit tests of UTC1 passed.");
    }
}
