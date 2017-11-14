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

    public void extend (K newKey, V newVal) {
        if (newKey.equals(this.key))
            this.val = newVal;
        else if (newKey.compareTo(this.key) < 0) {
            if (left == null) {
                left = new NonEmptyAList(newKey, newVal, null, null);
            }
            else left.extend(newKey,val);
        }
        else {if (right == null) {
                right = new NonEmptyAList(newKey, newVal, null, null);
            }
            else right.extend(newKey,val);
        }
    }

    // Returns true iff the key is found within this AList.

    public boolean contains (K key) {
	if (key.equals (this.key))
	    return true;
	else if (key.compareTo(this.key) < 0) 
                 return (left != null && left.contains(key));
        else
            return (right != null && right.contains(key));
                
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

    private String maybeToString (AList<K,V> x) {
        if (x == null) {return "%";}
        else {return x.toString();}
    }
        

    // We will not have reason to compare ALists to each other, so we
    // won't stop to override equals, toString, or hashcode.

    public String toString() {
        return ("[" +
                maybeToString(left)
                + (this.key.toString())
                + ":" + (this.val.toString())
                + maybeToString(right)
                + "]");
    }

}
