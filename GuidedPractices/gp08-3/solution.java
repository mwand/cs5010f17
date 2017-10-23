// to compile and run this file, say
// javac <thisfilename>.java
// java -enableassertions BinSearch
//
// (Note: By default, assertions are disabled in Java.)

// no objects here, all we have is a single class with some
// fields and static methods

// In Java, array A runs from 0 to A.length-1.

// The following definition will make our purpose statements easier to read.

// DEFINITION: A is _nondecreasing_ iff
//            (0 <= i <= j <= A.length) implies A[i] <= A[j])
// Note that this allows duplicate elements.
 

class BinSearch {

    static int binsearch_recursive (int[]A, int tgt) {

        // GIVEN: an array of ints A and a target 'tgt'
        // WHERE: A is non-decreasing
        // RETURNS: a number k such that
        //         0 <= k < A.length
        //     and A[k] = tgt
        // if there is such a k,
        //         otherwise returns -1
        // STRATEGY: Call a more general function

        return recursive_loop (0, A.length, A, tgt);
    }

    static int recursive_loop (int lo, int hi, int[] A, int tgt) {

        // GIVEN: two integers lo and hi, an array of ints A, and a target tgt
        // WHERE: A is non-decreasing
        // AND    0 <= lo <= hi <= A.length
        // AND    (forall j)(0  <= j < lo       ==> A[j] < tgt)
        // AND    (forall j)(hi <= j < A.length ==> A[j] > tgt)
        
        // RETURNS: a number k such that lo <= k < hi and f(k) = tgt if
        //     there is such a k, otherwise -1.

        // HALTING MEASURE: hi-lo
        // JUSTIFICATION: hi-lo is guaranteed to be non-negative,
        // since the invariant tells us that lo <= hi,
        // and it decreases at every recursive call because one of the
        // following happens:
        //     lo increases while hi stays the same
        //     lo stays the same while hi decreases

        if (lo == hi) {
            // the search area is empty
            return -1;
        }
        else { /* do nothing */}
        // choose an element in [lo,hi) . 
        int mid = (lo + hi) / 2;
        // mid will be less than hi because quotient truncates down.
        if (A[mid] == tgt) {
            // we have found the target
            return mid;
        }
        else if (A[mid] < tgt) {
            // the target can't be to the left of mid, so search right half
            return recursive_loop (mid+1, hi, A, tgt);
        }
        else {
            // otherwise the target can't be to the right of mid, so
            // search left half.
            return recursive_loop (lo, mid, A, tgt);
        }
    }

    
    static int binsearch_iterative (int[] A, int tgt) {

        // GIVEN: An array A of integers and an integer target 'tgt'
        // WHERE: A is non-decreasing
        // RETURNS: a number k such that
        //         0 <= k < A.length
        //     and A[k] = tgt
        // if there is such a k, otherwise returns -1

        int lo = 0;        
        int hi = A.length;

        // INVARIANT:
        //       0 <= lo <= hi <= A.length
        // AND   (forall j)(0  <= j < lo       ==> A[j] < tgt)
        // AND   (forall j)(hi <= j < A.length ==> A[j] > tgt)

        // Note that lo = 0 and hi = A.length makes the invariant
        // true, since in both cases there is no such j.

        // HALTING MEASURE: hi-lo
        // JUSTIFICATION: Same as above.

        while (lo < hi) {       // the search area is non-empty
            // choose an element in [lo,hi) . 
            int mid = (lo + hi) / 2;
            if (A[mid] == tgt) {
                // we have found the target
                return mid;
            }
            else if (A[mid] < tgt) {
                // the target can't be to the left of mid, so search
                // right half. 
                lo = mid+1;
            }
            // otherwise the target can't be to the right of mid,
            // so search left half. 
            else
                hi = mid;
        }

        // the search area is empty
        return -1;
    }

    static int count_occurrences (int[] A, int tgt) {

        // GIVEN: A non-decreasing array A of integers and an integer
        // target 'tgt'
        // RETURNS: the number of occurrences of tgt in A.

        int p = binsearch_recursive (A, tgt);
        if (p == -1) { // no occurrences of tgt in the array
            return 0;
        } else { /* do nothing */}

        int lo = p;
        int hi = p;

        // INVARIANT: lo, hi in [0, A.length)
        // AND if j in [lo,hi], then A[j] = tgt

        // expand lo downwards -- can we safely decrement lo?
        // HALTING MEASURE: lo
        // JUSTIFICATION: the invariant says that lo is non-negative,
        // and lo decreases every time through the loop.
        while (lo > 0 && A[lo-1]==tgt) {lo = lo - 1;};

        // expand hi upwards -- can we safely increment hi?
        // HALTING MEASURE: (A.length - hi)
        // JUSTIFICATION: the invariant hi in [0,A.length) implies
        // that hi is < A.length, hence (A.length - hi) is
        // non-negative.  Furthermore, hi increases every time through
        // the loop, so (A.length - hi) decreases every time through
        // the loop.
        while (hi < A.length-1 && A[hi+1]==tgt) {hi = hi+1;};
        return (hi - lo + 1);
    }
        

    public static void main (String[] args) {
        int[] A = {0,1,4,9,16,25,36,49};
        int[] B = {0,1,2,3,3,8,9,10,10,12};
        int[] C = {};
        int[] D = {1,3,5,5,5,6,6,7,8,8,8,8};
        int[] E = {7,7,7,7};
            

        assert binsearch_recursive(A,9) == 3;
        assert binsearch_recursive(A,10) == -1;
        assert binsearch_recursive(A,-5) == -1;
        assert binsearch_recursive(A,100) == -1;
        assert (binsearch_recursive(B,3) == 3 || binsearch_recursive(B,3) == 4);
        assert binsearch_recursive(C,17) == -1;

        assert binsearch_iterative(A,9) == 3;
        assert binsearch_iterative(A,10) == -1;
        assert binsearch_iterative(A,-5) == -1;
        assert binsearch_iterative(A,100) == -1;
        assert (binsearch_iterative(B,3) == 3 || binsearch_iterative(B,3) == 4);
        assert binsearch_iterative(C,17) == -1;

        // try using PdpTestSuite
        // timeout of 10 seconds
        PdpTestSuite tests = new PdpTestSuite(10);

               tests.addTestCase("recursive (A,9) => 3",
                          () -> binsearch_recursive(A,9) == 3,
                          true);

               // should have all the other tests here as well...

               tests.addTestCase("1 = 2 (should fail)",
                          () -> 1 == 2,
                          true);

        tests.addTestCase("3 = 3",
                          () -> 3 == 3,
                          true);

        tests.addTestCase("no occurrence of tgt",
                          () -> count_occurrences(D,17) == 0,
                          true);

        tests.addTestCase("three occurrences of 5 in D",
                          () -> count_occurrences(D,5) == 3,
                          true);

        tests.addTestCase("one occurrence of 7 in D",
                          () -> count_occurrences(D,7) == 1,
                          true);

        tests.addTestCase("tgt area reaches left end",
                          () -> count_occurrences(D,1) == 1,
                          true);

        tests.addTestCase("tgt area reaches right end",
                          () -> count_occurrences(D,8) == 4,
                          true);

        tests.addTestCase("tgt area is the whole array",
                          () -> count_occurrences(E,7),
                          4);

            

            

        tests.runTests();

    }
}
