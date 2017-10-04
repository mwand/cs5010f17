// A Flight is an object of any class that implements Flight.
//
// Interpretation: A Flight represents an airline flight that
// encapsulates the information supplied by the operations
// listed in this interface.

interface Flight {

    // Returns the name of this flight.

    String name ();

    // Returns the name of the airport from which this flight departs.

    String departs ();

    // Returns the name of the airport at which this flight arrives.

    String arrives ();

    // Returns the time at which this flight departs.

    UTC departsAt ();

    // Returns the time at which this flight arrives.

    UTC arrivesAt ();

    // Returns true iff this flight and the given flight
    //     have the same name
    //     depart from the same airport
    //     arrive at the same airport
    //     depart at the same time
    // and arrive at the same time

    boolean isEqual (Flight f2);
}
