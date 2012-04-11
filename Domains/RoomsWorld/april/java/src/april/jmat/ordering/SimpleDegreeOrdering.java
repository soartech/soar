package april.jmat.ordering;

import april.jmat.*;
import java.util.*;

/** This is not MinimumDegreeOrdering: it considers only the degree of
 * the nodes without the effects of marginalization.
 **/
public class SimpleDegreeOrdering implements Ordering
{
    static class Node implements Comparable<Node>
    {
        int index;
        int neighbors;

        public int compareTo(Node n)
        {
            return neighbors - n.neighbors;
        }
    }

    public SimpleDegreeOrdering()
    {
    }

    public int[] getPermutation(Matrix A)
    {
        ArrayList<Node> nodes = new ArrayList<Node>();

        int m = A.getRowDimension();
        int n = A.getColumnDimension();

        for (int col = 0; col < n; col++) {
            Node node = new Node();
            node.index = col;
            nodes.add(node);
        }

        for (int row = 0; row < m; row++) {
            for (int col = 0; col < n; col++) {
                if (A.get(row, col)!=0)
                    nodes.get(col).neighbors++;
            }
        }

        Collections.sort(nodes);
        int perm[] = new int[nodes.size()];

        for (int i = 0; i < nodes.size(); i++) {
            perm[i] = nodes.get(i).index;
        }

        return perm;
    }
}
