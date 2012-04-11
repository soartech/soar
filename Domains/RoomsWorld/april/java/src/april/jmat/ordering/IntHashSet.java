package april.jmat.ordering;

/** A class for very quickly computing sets composed of integers. We
    assume the maximum range of the integers is relatively small.
**/
public final class IntHashSet
{
    /** currentValue is never zero **/
    short  currentValue = 1;

    /** if values[i]==currentValue, then i is a member of the
     * set. Invariant: capacity is a power of two.
     **/
    short  values[] = new short[16];

    /** how many elements in the set? **/
    public int size;

    /** A list of the members of this set, unsorted. **/
    public int members[] = new int[values.length];

    public IntHashSet()
    {
    }

    public final void reset()
    {
        size = 0;
        currentValue++;

        // handle wrap around
        if (currentValue == 0) {
            // must rewrite entire index to avoid potential collisions.
            for (int i = 0; i < values.length; i++)
                values[i] = 0;
            currentValue = 1;
        }
    }

    /** Increase the maximum representable integer value, preserving
     * the state of the hash set.
     **/
    public final void ensureCapacity(int maxIndex)
    {
        if (values.length <= maxIndex) {
            int newsize = values.length;
            while (newsize <= maxIndex)
                newsize *= 2;

            short newvalues[] = new short[newsize];
            System.arraycopy(values, 0, newvalues, 0, values.length);
            values = newvalues;
            // enlarged values are zero, which is never equal to currentValue.

            int newmembers[] = new int[newsize];
            System.arraycopy(members, 0, newmembers, 0, size);
            members = newmembers;
        }
    }

    /** Size of the set **/
    public final int getSize()
    {
        return size;
    }

    /** Prevent index v from being added to the set. If v has already
     * been added, it will NOT be removed.
     **/
    public final void ban(int v)
    {
        values[v] = currentValue;
    }

    /** Add integer v. Returns true if the element was newly added,
     * false if the element was already present or banned. You must
     * ensureCapacity(w) with w &gt;= v.
     **/
    public final boolean add(int v)
    {
        if (values[v] != currentValue) {
            values[v] = currentValue;
            members[size] = v;
            size++;
            return true;
        }
        return false;
    }

    public final boolean contains(int v)
    {
        return (values[v] == currentValue);
    }
}
