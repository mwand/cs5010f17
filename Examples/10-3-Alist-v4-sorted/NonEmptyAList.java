// The object constructed by NonEmptyAlst(k,v,rest) represents an
// association list just like 'rest', except that the key k is mapped
// to the value v.  Any other key is mapped to the value it had in 'rest'.

class NonEmptyAList<K extends Comparable<K>,V> implements AList<K,V> {

    private K key; // key for the first entry in this AList
    private V val; // val for the first entry in this AList
    private AList<K,V> rest; // the other entries in this AList

    NonEmptyAList (K key, V val, AList<K,V> rest) {
	this.key = key;
	this.val = val;
	this.rest = rest;
    }

    // Returns an association list like this one
    // except the given key is associated with the given value.

    // maintains the list in sorted order
    // use <= so to maintain stable sort for duplicate keys :)

    public AList<K,V> extend (K newKey, V newVal) {
        
        if (newKey.compareTo(this.key) <= 0)
            return new NonEmptyAList<K,V> (newKey, newVal, this);
        else
            return new NonEmptyAList<K,V>
                (this.key,
                 this.val,
                 this.rest.extend(newKey,newVal));
    }

    // Returns true iff the key is found within this AList.

    public boolean contains (K otherKey) {
        if (otherKey.compareTo(this.key) < 0) {return false;}
        else if (otherKey.equals (this.key))
	    return true;
	else
	    return rest.contains (otherKey);
    }
    
    // Returns the value associated with the given key.
    // Throws a NoSuchElementException if the key is not found.

    public V lookup (K key) {
	if (key.equals (this.key))
	    return this.val;
	else
	    return rest.lookup (key);
    }

    // We will not have reason to compare ALists to each other, so we
    // won't stop to override equals, toString, or hashcode.

        public String toString () {
            return ("["
                    + this.key.toString() +":" + this.val.toString()
                    + "]" + this.rest.toString()
                    );
        }

}
