// A SortedIMap<K,V> is an object of any class that implements SortedIMap<K,V>.
//
// Interpretation: A SortedIMap<K,V> represents an immutable finite function
// from keys to values, together with a comparator that defines a total
// ordering on the keys.
//
// Constraint: the extend method of a SortedIMap<K,V> must return
// a SortedIMap<K,V>.

import java.util.SortedMap;

interface SortedIMap<K,V> extends SortedMap<K,V>, IMap<K,V> {
}
