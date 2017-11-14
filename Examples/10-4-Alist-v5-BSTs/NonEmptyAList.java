// The object constructed by NonEmptyAlst(k,v,rest) represents an
// association list just like 'rest', except that the key k is mapped
// to the value v.  Any other key is mapped to the value it had in 'rest'.

// We implement this as a binary search tree.

class NonEmptyAList<K extends Comparable<K>,V> implements AList<K,V> {

    private K key; // key for this entry in the AList
    private V val; // val for this entry in the AList
    private AList<K,V> left; // the entries with keys < this key
    private AList<K,V> right; // the entries with keys > this key

    NonEmptyAList (K key, V val, AList<K,V> left, AList<K,V> right) {
	this.key = key;
	this.val = val;
	this.left = left;
        this.right = right;
    }

    // Returns an association list like this one
    // except the given key is associated with the given value.
    // Be smart about duplicate keys.
    // New keys are eventually inserted at the leaves

    public AList<K,V> extend (K newKey, V val) {
        if (newKey.equals(this.key))
            return new NonEmptyAList<K,V>
                (this.key, val, left, right);
        if (newKey.compareTo(this.key) <= 0)
            return new NonEmptyAList<K,V>
                (this.key, this.val,
                 left.extend(newKey,val),
                 right);
        else
            return new NonEmptyAList<K,V>
                (this.key, this.val,
                 left,
                 right.extend(newKey,val));
    }

    // Returns true iff the key is found within this AList.

    public boolean contains (K key) {
	if (key.equals (this.key))
	    return true;
	else if (key.compareTo(this.key) < 0)
	    return left.contains(key);
        else
            return right.contains(key);
                
    }
    // Returns the value associated with the given key.
    // Throws a NoSuchElementException if the key is not found.

    public V lookup (K key) {
	if (key.equals (this.key))
	    return this.val;
	else if (key.compareTo(this.key) < 0)
            return left.lookup(key);
        else return right.lookup(key);
    }

    // We will not have reason to compare ALists to each other, so we
    // won't stop to override equals, toString, or hashcode.

    public String toString() {
        return ("[" +
                left.toString()
                + (this.key.toString())
                + ":" + (this.val.toString())
                + right.toString()
                + "]");
    }

}
