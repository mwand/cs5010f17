// Constructor templates for IMapList<K,V>:
//     new IMapList<K,V> (rktList)
//     new IMapList<K,V> ()
// Interpretation:
//     rktList is a RacketList<SimpleImmutableEntry<K,V>
//         containing the key/value pairs of the IMap
//         where no key is the key for two different key/value pairs
//     if rktList is omitted, creates an empty map

import java.util.Map;
import java.util.Map.Entry;
import java.util.AbstractMap;
import java.util.AbstractMap.SimpleEntry;
import java.util.AbstractMap.SimpleImmutableEntry;

import java.util.Set;
import java.util.AbstractSet;

import java.util.Iterator;

// Strategy, quoted from the documentation for AbstractMap:
//
// To implement an unmodifiable map, the programmer needs only to
// extend [the AbstractMap] class and provide an implementation
// for the entrySet method, which returns a set-view of the map's
// mappings.  Typically, the returned set will, in turn, be
// implemented atop AbstractSet.  This set should not support the
// add or remove methods, and its iterator should not support the
// remove method.

class IMapList<K,V> extends AbstractMap<K,V> implements IMap<K,V> {

    RacketList<Entry<K,V>> entries;    // set of key/value pairs

    // Java constructors

    IMapList () {
        this.entries = RacketLists.empty();
    }

    IMapList (RacketList<Entry<K,V>> entries) {
        this.entries = entries;
    }

    // public methods listed by IMap<K,V> interface

    // Returns an IMap like this one except the given key is associated
    // with the given value.  The extend operation does not modify this
    // IMap.

    public IMap<K,V> extend (K key, V value) {
        Entry<K,V> entry = new SimpleImmutableEntry<K,V> (key, value);
        return new IMapList<K,V> (extended (entries, entry));
    }

    // public methods listed by Map<K,V> interface

    // Returns a set of the key/value pairs in this Map.

    public Set<Map.Entry<K,V>> entrySet () {
        return new IMapListSet<K,V> (entries);
    }

    // all other public methods are defined by AbstractMap<K,V>

    // private help methods

    // Given a RacketList of entries and an entry with key k,
    // returns a RacketList in which the given entry replaces
    // any entry with key k or, if the given RacketList of entries
    // contains no entry with key k, returns a RacketList with
    // the given entry added to it.
    //
    // This method has no side effects.

    private RacketList<Entry<K,V>> extended (RacketList<Entry<K,V>> entries,
                                             Entry<K,V> entry) {
        if (entries.isEmpty()) {
            return entries.cons (entry);
        }
        if (entries.first().getKey().equals (entry.getKey())) {
            return entries.rest().cons (entry);
        }
        else
            return extended (entries.rest(), entry).cons (entries.first());
    }
}

class IMapListSet<K,V> extends AbstractSet<Entry<K,V>> {

    RacketList<Entry<K,V>> entries;

    // Java constructors

    IMapListSet () {
        this.entries = RacketLists.empty();
    }

    IMapListSet (RacketList<Entry<K,V>> entries) {
        this.entries = entries;
    }

    // public methods

    // Returns an Iterator for the entries in this set.

    public Iterator<Entry<K,V>> iterator () {
        return RacketLists.makeIterator (entries);
    }

    // Returns the size of this set.

    public int size () { return RacketLists.length (entries); }
}
