package april.newvis;

import java.util.*;

public interface VisLayerLayoutManager
{
    public static class Position
    {
        public VisLayer layer;
        public int viewport[];
    }

    public ArrayList<Position> layout(int viewport[]);
}
