package april.jmat;

import java.util.*;

public class CSRAllocator
{
    ArrayList<ArrayList<CSRVec>> cache = new ArrayList<ArrayList<CSRVec>>();

    public CSRAllocator()
    {
    }

    final ArrayList<CSRVec> getBucket(int capacity, int up)
    {
        int bucketidx;

        if (capacity < 1024)
            bucketidx = capacity/32;
        else if (capacity < 32768)
            bucketidx = (capacity/1024) + 32;
        else
            bucketidx = capacity/32768 + 64;

        bucketidx += up;

        while (bucketidx >= cache.size())
            cache.add(new ArrayList<CSRVec>());

        return cache.get(bucketidx);
    }

    public CSRVec get(int length, int mincapacity)
    {
        ArrayList<CSRVec> bucket = getBucket(mincapacity, 1);
        if (bucket.size() > 0) {
            CSRVec res = bucket.get(bucket.size()-1);
            bucket.remove(bucket.size()-1);

            res.length = length;
            res.nz = 0;
            return res;
        }

        return new CSRVec(length, mincapacity);
    }

    public void put(CSRVec v)
    {
        ArrayList<CSRVec> bucket = getBucket(v.indices.length, 0);
        if (bucket.size() < 10)
            bucket.add(v);
    }
}
