// Abstract base class for classes that implement the UTC interface.

abstract class UTC0 implements UTC {

    // the Java constructor has no fields to initialize
    // (all fields will be defined in concrete subclasses)

    UTC0 () { }

    // public methods

    // Returns the hour, between 0 and 23 inclusive.

    public abstract int hour ();

    // Returns the minute, between 0 and 59 inclusive.

    public abstract int minute ();

    // Returns true iff the given UTC is equal to this UTC.

    public boolean isEqual (UTC t2) {
        return (this.hour() == t2.hour()) && (this.minute() == t2.minute());
    }

    // public methods that override methods inherited from Object

    public boolean equals (Object x) {
        if (x instanceof UTC) {
            UTC t2 = (UTC) x;
            return isEqual (t2);
        }
        else return false;
    }

    public int hashCode () {
        return 100 * this.hour() + this.minute();
    }

    public String toString () {
        return ((Integer) (100 * this.hour() + this.minute())).toString();
    }
}
