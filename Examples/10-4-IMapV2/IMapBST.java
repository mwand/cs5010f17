// Constructor templates for IMapBST<K,V>:
//
//     IMapBST.empty(c)
// Interpretation:
//     c is the comparator to be used by the map
//
//     this.make (k, v, left, right)
// Interpretation:
//     k and v are the key and value to be used at the root
//     left is the left subtree
//     right is the right subtree
// Where:
//     k is distinct from all keys in the left and right subtrees
//     k is greater than all keys in the left subtree
//     k is less than all keys in the right subtree
//     the left and right subtrees obey those invariants, recursively

import java.util.List;
import java.util.ArrayList;

import java.util.Iterator;

import java.util.Comparator;

// Strategy: extends SortedIMapBase<K,V>

abstract class IMapBST<K,V> extends SortedIMapBase<K,V> {

    private Comparator<? super K> cmp; // the comparator for this map

    IMapBST (Comparator<? super K> cmp) {
        this.cmp = cmp;
    }

    // Returns an empty map with the given comparator.

    static <K,V> IMapBST<K,V> empty (Comparator<? super K> cmp) {
        return new IMapEmptyBST<K,V> (cmp);
    }

    // Returns an non-empty map with the given components.
    // Invariants:
    //     key does not occur in the left or right subtrees
    //     key is greater than all keys in left subtree
    //     key is less than all keys in right subtree
    //     the left and right subtrees obey those invariants, recursively

    IMapBST<K,V> make (K key, V val,
                       IMapBST<K,V> left,
                       IMapBST<K,V> right) {
        return new IMapNonEmptyBST<K,V> (key, val, left, right);
    }

    // abstract methods listed by SortedMap<K,V>

    public Comparator<? super K> comparator () { return cmp; }

    // Returns an iterator for the keys of this IMap.

    protected Iterator<K> keyIterator() {
        List<K> lst = new ArrayList<K>();
        this.addKeys (lst);
        return lst.iterator();
    }

    // Abstract help methods.

    // Adds the keys of this tree, in order, to the given list.

    abstract void addKeys (List<K> lst);
}

class IMapEmptyBST<K,V> extends IMapBST<K,V> {

    // Java constructor

    IMapEmptyBST (Comparator<? super K> cmp) {
        super(cmp);
    }

    // abstract methods listed by Map<K,V>

    public int size () { return 0; }

    // abstract methods listed by IMap<K,V>

    public IMap<K,V> extend (K key, V val) {
        return this.make (key, val, this, this);
    }

    // abstract methods declared by IMapBase<K,V>

    // Returns true iff this map has a mapping for the given key.

    protected boolean hasKey (K key) { return false; }

    // Returns the value associated with the given key.

    protected V getVal (K key) {
        throw new RuntimeException ("getVal called with empty map: " + key);
    }

    // Abstract help methods.

    // Adds the keys of this tree, in order, to the given list.

    void addKeys (List<K> lst) { }
}

class IMapNonEmptyBST<K,V> extends IMapBST<K,V> {

    private K key;                // the key at the root of this tree
    private V val;                // the value associated with that key
    private IMapBST<K,V> left;    // the left subtree of this tree
    private IMapBST<K,V> right;   // the right subtree of this tree

    private int theSize;          // the size of this tree

    // Java constructor
    // Invariant: the left and right subtrees have the same comparator.

    IMapNonEmptyBST (K key, V val, IMapBST<K,V> left, IMapBST<K,V> right) {
        super(left.comparator());
        this.key = key;
        this.val = val;
        this.left = left;
        this.right = right;
        this.theSize = 1 + left.size() + right.size();
    }

    // abstract methods listed by Map<K,V>

    public int size () { return theSize; }

    // abstract methods listed by IMap<K,V>

    // Returns an IMap like this one except the given key is associated
    // with the given value.  The extend operation does not modify this
    // IMap.
    //
    // Invariant: the key cannot be null.

    public IMap<K,V> extend (K key, V val) {
        int cresult = this.comparator().compare (key, this.key);
        if (cresult == 0)
            return this.make (key, val, left, right);
        else if (cresult < 0)
            return this.make (this.key, this.val,
                              (IMapBST<K,V>) this.left.extend (key, val),
                              this.right);
        else
            return this.make (this.key, this.val,
                              this.left,
                              (IMapBST<K,V>) this.right.extend (key, val));
    }

    // abstract methods declared by IMapBase<K,V>

    // Returns true iff this map has a mapping for the given key.

    protected boolean hasKey (K key) {
        int cresult = this.comparator().compare (key, this.key);
        if (cresult == 0)
            return true;
        else if (cresult < 0)
            return this.left.hasKey(key);
        else
            return this.right.hasKey(key);
    }

    // Returns the value associated with the given key.

    protected V getVal (K key) {
        int cresult = this.comparator().compare (key, this.key);
        if (cresult == 0)
            return this.val;
        else if (cresult < 0)
            return this.left.getVal(key);
        else
            return this.right.getVal(key);
    }

    // Abstract help methods.

    // Adds the keys of this tree, in order, to the given list.

    void addKeys (List<K> lst) {
        this.left.addKeys (lst);
        lst.add (this.key);
        this.right.addKeys (lst);
    }
}
