// Abstract base class for classes that implement SortedIMap<K,V>.
//
// To define a class that implements SortedIMap<K,V>:
//     make that class a subclass of SortedIMapBase<K,V>
//     within that class, define the following abstract methods:
//         public    Comparator<? super K> comparator()
//         public    IMap<K,V>             extend(K key, V value)
//         protected boolean               hasKey(K key)
//         protected V                     getVal(K key)
//         protected Iterator<K>           keyIterator()
//     within that class, override the following methods:
//         public    int                   size()

import java.util.Map;
import java.util.Map.Entry;
import java.util.AbstractMap;
import java.util.AbstractMap.SimpleEntry;
import java.util.AbstractMap.SimpleImmutableEntry;

import java.util.SortedMap;

import java.util.Set;
import java.util.AbstractSet;

import java.util.List;
import java.util.ArrayList;

import java.util.Iterator;

import java.util.Comparator;

import java.util.NoSuchElementException;

// Strategy: extend IMapBase<K,V> and add the necessary methods.

abstract class SortedIMapBase<K,V>
    extends IMapBase<K,V>
    implements SortedIMap<K,V> {

    // Java constructor

    protected SortedIMapBase () { }

    // public methods listed by SortedMap<K,V> interface

    // Returns the comparator used to sort the keys in this map.

    public abstract Comparator<? super K> comparator();

    // public methods listed by the SortedMap<K,V> interface

    // Returns the first (lowest) key in this map.

    public K firstKey() {
        if (this.isEmpty())
            throw new NoSuchElementException (emptySortedMapMsg);
        else
            return this.keyIterator().next();
    }

    // Returns the last (highest) key in this map.

    public K lastKey() {
        if (this.isEmpty())
            throw new NoSuchElementException (emptySortedMapMsg);
        else {
            Iterator<K> it = this.keyIterator();
            K result = it.next();
            while (it.hasNext())
                result = it.next();
            return result;
        }
    }

    private static final String emptySortedMapMsg
        = "firstKey or lastKey called on empty SortedIMap";

    // Returns a view of the portion of this map whose keys are
    // strictly less than toKey.

    public SortedMap<K,V> headMap (K toKey) {
        Comparator<? super K> c = this.comparator();
        Iterator<K> it = this.keyIterator();
        SortedIMap<K,V> result = IMaps.empty(c);
        while (it.hasNext()) {
            K key = it.next();
            V val = this.getVal(key);
            if (c.compare (key, toKey) < 0)
                result = (SortedIMap<K,V>) result.extend (key, val);
            else
                return result;
        }
        return result;
    }

    // Returns a view of the portion of this map whose keys range
    // from fromKey, inclusive, to toKey, exclusive.

    public SortedMap<K,V> subMap (K fromKey, K toKey) {
        Comparator<? super K> c = this.comparator();
        Iterator<K> it = this.keyIterator();
        SortedIMap<K,V> result = IMaps.empty(c);
        while (it.hasNext()) {
            K key = it.next();
            V val = this.getVal(key);
            if (c.compare (key, fromKey) < 0)
                ; // do nothing
            else if (c.compare (key, toKey) < 0) 
                result = (SortedIMap<K,V>) result.extend (key, val);
            else
                return result;
        }
        return result;
    }

    // Returns a view of the portion of this map whose keys are
    // greater than or equal to fromKey.

    public SortedMap<K,V> tailMap (K fromKey) {
        Comparator<? super K> c = this.comparator();
        Iterator<K> it = this.keyIterator();
        SortedIMap<K,V> result = IMaps.empty(c);
        while (it.hasNext()) {
            K key = it.next();
            V val = this.getVal(key);
            if (c.compare (key, fromKey) < 0)
                ; // do nothing
            else
                result = (SortedIMap<K,V>) result.extend (key, val);
        }
        return result;
    }

    // all other public methods are defined by IMapBase<K,V>

    // overriding the usual triumvirate

    // Sorted maps are equal if they have equal keys and values.
    // Sorted maps are never considered equal to unsorted maps
    // because the existence of a comparator method creates an
    // observable difference, and the comparator might not be
    // consistent with the equals method on keys.

    @SuppressWarnings("unchecked")
    public boolean equals (Object x) {
        if (x instanceof SortedIMapBase) {
            SortedIMapBase<K,V> m = (SortedIMapBase<K,V>) x;
            if (! (this.comparator().equals(m.comparator())))
                return false;
            else if (this.size() != m.size())
                return false;
            else {
                Iterator<K> it = keyIterator();
                while (it.hasNext()) {
                    K key = it.next();
                    if (m.hasKey(key) &&
                        this.getVal(key).equals(m.getVal(key)))
                        ; // do nothing
                    else return false;
                }
                return true;
            }
        }
        else return false;
    }

    // The hash code of a sorted map can depend on the order
    // in which keys are generated by the iterator, but this
    // definition doesn't take advantage of that.

    public int hashCode () {
        Iterator<K> it = keyIterator();
        int result = 2005052521;
        while (it.hasNext()) {
            K key = it.next();
            V val = getVal (key);
            int keyHash = key.hashCode();
            int valHash = val.hashCode();
            result = result - 978278598 * keyHash + 1048370044 * valHash;
        }
        return result;
    }

    public String toString () {
        System.out.println (super.toString());
        return "a SortedIMap of size " + size() + " and hash " + hashCode();
    }
}
