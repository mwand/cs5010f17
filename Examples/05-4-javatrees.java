// compile and run this file, say
// javac 05-4-javatrees
// java BinTree_Tests

import static java.lang.Math.*;

// a BinTree is an object of any class that implements BinTree.

interface BinTree {

    int leaf_sum (); // returns the sum of the values in the leaves
    int leaf_max (); // returns the largest value in a leaf of the tree
    int leaf_min (); // returns the smallest value in a leaf of the tree

}

class Leaf implements BinTree {
    int datum;   // some integer data
    Leaf (int n) {
	datum = n;
    }

    public int leaf_sum () {return datum;}
    public int leaf_max () {return datum;}
    public int leaf_min () {return datum;}

}

class Node implements BinTree {
    BinTree lson, rson;   // the left and right sons

    Node (BinTree l, BinTree r) {
	lson = l ; rson = r;}

    public int leaf_sum () {
	return (lson.leaf_sum() + rson.leaf_sum());
    }

    public int leaf_max () {
	return (max (lson.leaf_max(), rson.leaf_max()));
    }

    public int leaf_min () {
	return (min (lson.leaf_min(), rson.leaf_min()));
    }
}

class BinTree_Tests {

    public static void main (String[] args) {
	System.out.printf("Hello, World!\n");
	// values for tests
	BinTree leaf_13 = new Leaf (13);
	BinTree leaf_2 = new Leaf (2);
	BinTree leaf_8 = new Leaf (8);
	BinTree tree_1 = new Node(leaf_13, leaf_2);
	BinTree tree_2 = new Node(leaf_8, tree_1);
	BinTree tree_3 = new Node(tree_1, tree_2);

	// tests

	assert tree_1.leaf_sum() == 15;
	assert tree_1.leaf_max() == 13;
	assert tree_1.leaf_min() ==  2;

	assert tree_3.leaf_sum() == 38;
	assert tree_3.leaf_max() == 13;
	assert tree_3.leaf_min() ==  2;

	System.out.printf("All tests passed!");
    }
}


    
    
	
    
	
