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
import java.util.AbstractMap.SimpleImmutableEntry;

import java.util.List;
import java.util.ArrayList;

import java.util.Iterator;

// Strategy: extends IMapBase<K,V>

final class IMapList<K,V> extends IMapBase<K,V> {

    RacketList<Entry<K,V>> entries;    // set of key/value pairs

    // Java constructors

    IMapList () {
        this.entries = RacketLists.empty();
    }

    IMapList (RacketList<Entry<K,V>> entries) {
        this.entries = entries;
    }

    // abstract methods listed by Map<K,V> interface

    public int size () {
        return RacketLists.length (entries);
    }

    // abstract methods listed by IMap<K,V> interface

    // Returns an IMap like this one except the given key is associated
    // with the given value.  The extend operation does not modify this
    // IMap.

    public IMap<K,V> extend (K key, V value) {
        Entry<K,V> entry = new SimpleImmutableEntry<K,V> (key, value);
        return new IMapList<K,V> (extended (entries, entry));
    }

    // abstract methods declared in IMapBase<K,V>

    // Returns true iff this map has a mapping for the given key.

    protected boolean hasKey (K key) {
        RacketList<Entry<K,V>> lst = entries;
        while (! (lst.isEmpty())) {
            if (lst.first().getKey().equals (key))
                return true;
            lst = lst.rest();
        }
        return false;
    }

    // Returns the value associated with the given key.

    protected V getVal (K key) {
        RacketList<Entry<K,V>> lst = entries;
        while (! (lst.isEmpty())) {
            if (lst.first().getKey().equals (key))
                return lst.first().getValue();
            lst = lst.rest();
        }
        String badKeyMsg = "key not found in IMap<K,V>: ";
        throw new IllegalArgumentException(badKeyMsg + key);
    }

    // Returns an iterator for the keys of this IMap.

    protected Iterator<K> keyIterator() {
        RacketList<Entry<K,V>> lst = entries;
        List<K> keys = new ArrayList<K>();
        while (! (lst.isEmpty())) {
            keys.add (lst.first().getKey());
            lst = lst.rest();
        }
        return keys.iterator();
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
