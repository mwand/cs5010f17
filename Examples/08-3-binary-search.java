// to compile and run this file, say
// javac 08-3-binary-search.java
// java BinSearch

// no objects here, all we have is a single class with some static methods
// fields and static methods

class BinSearch {

    static int binsearch_recursive (int[]A, int tgt) 

    // GIVEN: an array of ints A and a target 'tgt'
    // WHERE: A is non-decreasing (ie, i <= j implies A[i] <= A[j])
    // RETURNS: a number k such that 0 <= k <= N and f(k) = tgt if there is such a k,
    //          otherwise -1
    // STRATEGY: Call a more general function

    {
	return recursive_loop (0, A.length, A, tgt);
    }

    static int recursive_loop (int lo, int hi, int[]A, int tgt)
	
    // GIVEN: two integers lo and hi, an array of ints A, and a target
    // tgt
    // WHERE: A is non-decreasing (ie, i <= j implies A[i] <= A[j])
    // RETURNS: a number k such that lo <= k <= hi and f(k) = tgt if
    // there is such a k, otherwise -1.

    // HALTING MEASURE: max((hi-lo),0)
    // JUSTIFICATION: (max (- hi lo) 0) is guaranteed non-negative,
    // and it decreases at every recursive call because p is 
    // eliminated.  Note that it is posible for lo to be greater than
    // hi if the target is not present in the array. See slides for more detailed argument.

    {
	if (lo > hi)
	    // the search area is empty
	    {return -1;};
	int mid = (lo + hi) / 2;
	 if (A[mid] == tgt)
		// we have found the target
	     {return mid;}
	    if (A[mid] < tgt)
		// the target can't be to the left of mid, so search right half
		{return recursive_loop (mid+1, hi, A, tgt);}
	    // otherwise the target can't be to the right of mid, so search left half
	    {return recursive_loop (lo, mid-1, A, tgt);}
    }

    
    static int binsearch_iterative (int[] A, int tgt) {
    // GIVEN: An array A of integers and an integer target 'tgt'
    // WHERE: A is sorted in ascending order (duplicates are possible)
    // RETURNS: a non-negative value i such that A[i] = tgt or -1 if no such i exists

	int lo = 0;
	int hi = A.length;

	// INVARIANT:
	// IF   there is some i in [0,A.length] such that A[i] = tgt
	// THEN there is some i in [lo,hi] such that A[i] = tgt

	// HALTING MEASURE: max((hi - lo),0)
	// JUSTIFICATION: Same as above.
    

	while (lo <= hi) { 		// the search area is non-empty
	    int mid = (lo + hi) / 2;
	    if (A[mid] == tgt)
		// we have found the target
		{return mid;}
	    if (A[mid] < tgt)
		// the target can't be to the left of mid, so search right half
		{lo = mid+1;};
	    // otherwise the target can't be to the right of mid, so search left half
	    {hi = mid - 1;}
	};

	// the search area is empty
	return -1;
    }


    public static void main (String[] args) {
	int[] A = {0,1,4,9,16,25,36,49};
	int[] B = {0,1,2,3,3,8,9,10,10,12};

	assert binsearch_recursive(A,9) == 3;
	assert binsearch_recursive(A,10) == -1;
	assert binsearch_recursive(A,-5) == -1;
	assert binsearch_recursive(A,100) == -1;
	assert (binsearch_recursive(B,3) == 3 || binsearch_recursive(B,3) == 4);

	assert binsearch_iterative(A,9) == 3;
	assert binsearch_iterative(A,10) == -1;
	assert binsearch_iterative(A,-5) == -1;
	assert binsearch_iterative(A,100) == -1;
	assert (binsearch_iterative(B,3) == 3 || binsearch_iterative(B,3) == 4);

    }
}
	    
	    
	
