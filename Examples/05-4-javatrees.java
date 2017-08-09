// a BinTree is an object of any class that implements BinTree.

interface BinTree {

    int sum (); // returns the sum of the values in the leaves
    int max (); // returns the largest value in a leaf of the tree
    int min (); // returns the smallest value in a leaf of the tree

}

class Leaf implements BinTree {
    int datum;   // some integer data
    Leaf (int n) {
	datum = n;
    }

    public int sum () {return n;}
    public int max () {return n;}
    public int min () {return n;}

}

class Node implements BinTree {
    BinTree lson, rson;   // the left and right sons

    Node (BinTree l, BinTree r) {
	lson = l ; rson = r;}

    public int sum () {
	return (lson.sum() + rson.sum());
    }

    public int max () {
	return (max (lson.max(), rson.max()));
    }

    public int min () {
	return (min (lson.min(), rson.min()));
    }
}

class BinTree_Tests {

    public static void main (String[] args) {
	System.out.printf("Hello, World!\n");
	BinTree leaf_13 = new Leaf (13);
	System.out.printf("Goodbye, cruel world!");
    }
}


    
    
	
    
	
