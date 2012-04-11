package april.graph;

import java.util.*;

public class Linearization
{
    public ArrayList<double[][]> J = new ArrayList<double[][]>(); // Jacobians with respect to the nodes connected by this edge
    public double R[];    // residual

    public double W[][];  // weight of linearization (information matrix)
}
