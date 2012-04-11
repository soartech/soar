package april.jmat;

/** Base type of all vectors (both sparse and dense). **/
public abstract class Vec
{
    /** Return an array corresponding to the vector's elements. This
     * may or may not be the internal representation of the vector, so
     * callers should not modify this array.
     **/
    public abstract double[] getDoubles();

    /** How long is the vector? **/
    public abstract int size();

    /** How many non-zero entries are there? **/
    public abstract int getNz();

    /** Get the element at index idx **/
    public abstract double get(int idx);

    /** Set the element at index idx to v. */
    public abstract void set(int idx, double v);

    /** Compute the dot product with vector r **/
    public abstract double dotProduct(Vec r);
    // dot product from [i0, i1]
    public abstract double dotProduct(Vec r, int i0, int i1);

    /** Make a copy of the vector **/
    public abstract Vec copy();

    public abstract double[] copyArray();

    /** Resize the vector, truncating or adding zeros as appropriate. **/
    public abstract void resize(int newlength);

    /** create a new, smaller vector beginning at element i0, going
	through i1 (inclusive). The length of this vector will be i1-i0+1.
    **/
    public abstract Vec copy(int i0, int i1);

    /** create a same-sized vector containing only the spec'd elements. **/
    public abstract Vec copyPart(int i0, int i1);

    /** Multiply all elements in the vector by v **/
    public abstract void timesEquals(double v);

    /** Multiply the elements between indices [i0,i1] (inclusive) by
     * v **/
    public abstract void timesEquals(double v, int i0, int i1);

    /** Set all elements to zero. **/
    public abstract void clear();

    /** Add the value v to each element. **/
    public void plusEquals(int idx, double v)
    {
	set(idx, get(idx) + v);
    }

    /** Add the vector v to the elements beginning at index idx **/
    public void plusEquals(int idx, double v[])
    {
	for (int i = 0; i < v.length; i++)
	    set(idx+i, get(idx+i) + v[i]);
    }

    /** sum of squared elements. **/
    public abstract double normF();

    /** Insert this vector as column 'col' in matrix A. The column is
	initially all zero. The vector should iterate through its
	elements, calling the matrix's set method.
    **/
    public abstract void transposeAsColumn(Matrix A, int col);

    /** Transpose only the elements at indices [i0,i1] inclusive. **/
    public abstract void transposeAsColumn(Matrix A, int col, int i0, int i1);

    /** Add this vector (scaled) to another vector. This vector
	is unchanged.
    **/
    public abstract void addTo(Vec r, double scale);
    public abstract void addTo(Vec r, double scale, int i0, int i1);

    /** reorder the columns of this matrix so that they are:
	X' = [ X(perm[0]) X(perm[1]) X(perm[2])... ]
    **/
    public abstract Vec copyPermuteColumns(Permutation p);
}
