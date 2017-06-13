// A RacketList<E> is an object of any class that implements RacketList<E>.
//
// Interpretation: A RacketList<E> represents an immutable
// (and possibly empty) sequence of E values, with operations
// analogous to those we've been using in Racket.

interface RacketList<E> {

    // Is this list empty?

    boolean isEmpty ();

    // WHERE: this list is non-empty
    // RETURNS: first element of this list

    E first ();

    // WHERE: this list is non-empty
    // RETURNS: rest of this list

    RacketList<E> rest ();

    // GIVEN: an arbitrary value x of type E
    // RETURNS: a list whose first element is x and whose
    //     rest is this list

    RacketList<E> cons (E x);
}
