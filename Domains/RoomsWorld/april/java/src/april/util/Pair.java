package april.util;

public class Pair<T1, T2>
{
    public T1 o1;
    public T2 o2;

    public Pair(T1 o1, T2 o2)
    {
        this.o1 = o1;
        this.o2 = o2;
    }

    public T1 getFirst()
    {
        return o1;
    }

    public T2 getSecond()
    {
        return o2;
    }

    public String toString()
    {
        return "(" + o1 + ", " + o2 + ")";
    }
}
