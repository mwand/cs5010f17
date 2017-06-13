// An IMap<K,V> is an object of any class that implements IMap<K,V>.
//
// Interpretation: An IMap<K,V> represents an immutable finite function
// from keys to values.
//
// IMap<K,V> extends java.util.Map<K,V>, so an IMap<K,V> can be used
// as an unmodifiable Map<K,V>.

import java.util.Map;

interface IMap<K,V> extends Map<K,V> {

    // Returns an IMap like this one except the given key is associated
    // with the given value.  The extend operation does not modify this
    // IMap.

    IMap<K,V> extend (K key, V value);
}
