// Objects of the EmptyAList class represent an empty
// association list with no keys.

import java.util.NoSuchElementException;

// An object of class EmptyAlist<K,V> represents an empty partial map
// from K to V, that is, a map with an empty domain.

// The representation is a header node root field, initially null, which will
// point at the eventual real root of the binary search tree.

import java.util.NoSuchElementException;



class Header<K extends Comparable<K>,V> implements AList<K,V> {

    AList<K,V> root = null;

    // Returns an association list like this one
    // except the given key is associated with the given value.

    // the first time, installs a new interior node as the value of
    // root.  This node will be the "real" root of the tree.
    // all subequent accessess are delegated to the root.

    public void extend (K key, V val) {
        if (root == null) {
            root = new NonEmptyAList<K,V> (key, val, null, null);
        }
        else root.extend(key,val);
    }

    // Returns true iff the key is found within this AList.

    public boolean contains (K key) {
        if (root == null) { return false; }
        else {return root.contains(key);}
    }

    // Returns the value associated with the given key.
    // Throws a NoSuchElementException if the key is not found.

    public V lookup (K key) {
        if (root == null) {
            throw new NoSuchElementException ("key not found: " + key);}
        else return root.lookup(key);
    }

    

    // We will not have reason to compare ALists to each other, so we
    // won't stop to override equals, toString, or hashcode.

    public String toString () {
        if (root == null) {return "*";}
        else {return "*"+root.toString();}
    }

}
