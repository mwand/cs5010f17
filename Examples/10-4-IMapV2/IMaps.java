// The IMaps class defines static methods for the IMap<K,V> type.
// In particular, it defines static factory methods for creating
// empty maps of the IMap<K,V> type.

import java.util.Iterator;

import java.util.Comparator;

class IMaps {

    private static IMap theEmptyIMap = new IMapList();

    // static factory methods for creating an empty IMap<K,V>

    @SuppressWarnings("unchecked")
    public static <K,V> IMap<K,V> empty () {
        return (IMap<K,V>) theEmptyIMap;
    }

    @SuppressWarnings("unchecked")
    public static <K,V> SortedIMap<K,V> empty (Comparator<? super K> c) {
        return IMapBST.empty(c);
    }

    // main method for testing

    public static void main (String[] args) {
        IMapTests.main (args);
    }
}

// Tests for IMap<K,V> objects.

class IMapTests {

    public static void main (String[] args) {
        mainUnsorted(args);
        mainSorted(args);
        System.out.println ("IMap tests passed.");
    }

    // Tests unsorted maps.

    private static void mainUnsorted (String[] args) {
        IMap<Integer,String> m0 = IMaps.empty();
        IMap<Integer,String> m00 = IMaps.empty();
        mainCommon (m0, m00);
    }

    // Tests sorted maps.

    private static void mainSorted (String[] args) {
        Comparator<Integer> cmp
            = new Comparator<Integer>() {
            public int compare (Integer i, Integer j) {
                return Integer.compare (i.intValue(), j.intValue());
            }
        };
        IMap<Integer,String> m0 = IMaps.empty(cmp);
        IMap<Integer,String> m00 = IMaps.empty(cmp);
        mainCommon (m0, m00);
    }

    // Works for both sorted and unsorted maps.

    private static void mainCommon (IMap<Integer,String> m0,
                                    IMap<Integer,String> m00) {
        IMap<Integer,String> m1 = m0.extend (1, "one");
        IMap<Integer,String> m2 = m0.extend (2, "two");
        IMap<Integer,String> m3 = m0.extend (3, "three");
        IMap<Integer,String> m4 = m0.extend (4, "four");
        IMap<Integer,String> m5 = m0.extend (5, "five");
        IMap<Integer,String> m6 = m0.extend (6, "six");
        IMap<Integer,String> m7 = m0.extend (7, "seven");
        IMap<Integer,String> m8 = m0.extend (8, "eight");
        IMap<Integer,String> m9 = m0.extend (9, "nine");

        IMap<Integer,String> m03 = m9.extend (3, "drei");
        IMap<Integer,String> m10 = m03.extend (10, "zehn");

        assert ! m0.containsKey(1);
        assert m9.containsKey(4);
        assert ! m9.containsKey(10);

        assert m9.get(9).equals("nine");
        assert m9.get(3).equals("three");
        assert m9.get(1).equals("one");

        assert m10.get(9).equals("nine");
        assert m10.get(3).equals("drei");
        assert m10.get(1).equals("one");

        assert m0.isEmpty();
        assert ! m1.isEmpty();

        assert m0.size() == 0;
        assert m1.size() == 1;
        assert m9.size() == 9;
        assert m03.size() == 9;
        assert m10.size() == 10;
    }
}
