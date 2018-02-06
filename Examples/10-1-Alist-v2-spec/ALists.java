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

    // this generates a new EmptyAList object every time it is called.
    public static <K,V> AList<K,V> empty () {
        return new EmptyAList<K,V> ();
    }
    
   public static void main (String[] args) {
       Tests.main (args);
   }

}


