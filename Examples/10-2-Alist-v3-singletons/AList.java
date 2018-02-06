// An AList<K,V> is an object of any class that implements
// the AList<K.V> interface.

// interpretation: An Alist<K,V> represents a finite partial map from K to V.

// Specification:

// Let a be an alist, k1 and k2 be unequal keys.  Then
// a.extend(k1,v1).lookup(k1) = v1
// a.extend(k1,v1).lookup(k2) = a.lookup(k2)

// a.extend(k1,v1).contains(k1) = true
// a.extend(k1,v1).contains(k2) = a.contains(k2)

// If empty_alist is a representation of the empty alist, then
// empty_alist.contains(k1) = false

// Note that this is what the English description says.  Furthermore
// the "=" means the Java equality predicate on the appropriate type.
// So, for example, the first equation above means

// a.extend(k1,v1).lookup(k1).equals(v1) is true

// Note also that the interface does NOT include a constructor for the
// empty AList. This is because different implementations of AList may
// require different arguments to this constructor.
    
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
