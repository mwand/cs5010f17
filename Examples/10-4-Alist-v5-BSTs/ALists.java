// To compile:
//
//     javac *.java
//
// To run:
//
//     java AListTests

// This class defines a main method for testing the EmptyAList class,
// which implements the AList interface.

class ALists {

    private static AList theEmptyAList = new EmptyAList();

    // static factory method for creating an empty AList<K,V>

    @SuppressWarnings("unchecked")
    public static <K extends Comparable<K>,V> AList<K,V> empty () {
        return (AList<K,V>) theEmptyAList;
    }


    // // this generates a new EmptyAList object every time it is called.
    // // public static <K,V> AList<K,V> empty () {
    // public static <K extends Comparable<K>,V> AList empty () {
    //     return new EmptyAList<K,V> ();
    // }
    
   public static void main (String[] args) {
       Tests.main (args);
   }

}


