// Objects of the EmptyAList class represent an empty
// association list with no keys.

import java.util.NoSuchElementException;

// An object of class EmptyAlist<K,V> represents an empty partial map
// from K to V, that is, a map with an empty domain.

class EmptyAList<K extends Comparable<K>,V> implements AList<K,V> {

    // Returns an association list like this one
    // except the given key is associated with the given value.

    public AList<K,V> extend (K key, V val) {
	return new NonEmptyAList<K,V> (key, val, this);
    }

    // Returns true iff the key is found within this AList.

    public boolean contains (K key) {
	return false;
    }

    // Returns the value associated with the given key.
    // Throws a NoSuchElementException if the key is not found.

    public V lookup (K key) {
	throw new NoSuchElementException ("key not found: " + key);
    }

    public String toString() {return "%";}

    // We will not have reason to compare ALists to each other, so we
    // won't stop to override equals, toString, or hashcode.

}
