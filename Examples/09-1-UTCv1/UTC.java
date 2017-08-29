// A UTC is an object of any class that implements UTC.
//
// Interpretation: A UTC represents a Universal Coordinated Time.

interface UTC {

    // Returns the hour, between 0 and 23 inclusive.

    int hour ();

    // Returns the minute, between 0 and 59 inclusive.

    int minute ();

    // Returns true iff the given UTC is equal to this UTC.

    boolean isEqual (UTC t2);
}
