package april.util;

import java.util.*;

public class BitHack
{
    protected static final int popCountTable8[] = new int[256];
    static {
        for (int i = 0; i < 256; i++)
            popCountTable8[i] = dumbPopCount(i);
    }

    protected static final int dumbPopCount(long v)
    {
        int popcount = 0;

        while (v != 0) {
            popcount++;
            v = v&(v-1);
        }

        return popcount;
    }

    public static final int popCount(int v)
    {
        int cnt = 0;

        cnt += popCountTable8[v&0xff];
        v >>= 8;

        cnt += popCountTable8[v&0xff];
        v >>= 8;

        cnt += popCountTable8[v&0xff];
        v >>= 8;

        cnt += popCountTable8[v&0xff];

        return cnt;
    }

    public static final int popCount(long v)
    {
        return popCount((int) v) + popCount((int) (v>>32));
    }

    public static void main(String args[])
    {
        Random r = new Random();

        assert(popCount(0)==0);
        assert(popCount(0x80000000)==1);
        assert(popCount(0xffffffff)==32);

        assert(popCount(0L)==0);
        assert(popCount(0x8000000000000000L)==1);
        assert(popCount(0xffffffffffffffffL)==64);

        assert(popCount(0x371)==6);
        assert(popCount(0x80000371)==7);
        assert(popCount(0x371L)==6);
        assert(popCount(0x8000000000000371L)==7);

        for (int i = 0; i < 100000; i++) {
            long v = r.nextLong();
            assert(dumbPopCount(v) == popCount(v));
        }
    }
}
