// An AList<K,V> is an object of any class that implements
// the AList<K.V> interface.

// interpretation: An Alist<K,V> represents a finite partial map from K to V.

interface AList<K,V> {

    // Returns an association list like this one
    // except the given key is associated with the given value.

    AList<K,V> extend (K key, V val);

    // Returns true iff the key is found within this AList.

    boolean contains (K key);

    // Returns the value associated with the given key.
    // Throws a NoSuchElementException if the key is not found.

    V lookup (K key);
}
