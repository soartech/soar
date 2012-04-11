package april.graph.convert;

import april.graph.*;
import april.jmat.*;

import java.io.*;

public class ImportVa
{
    public static void main(String args[])
    {
        Graph g = new Graph();

        try {
            BufferedReader ins = new BufferedReader(new FileReader(args[0]));
            String line = null;

            while ((line = ins.readLine()) != null) {
                String toks[] = line.split("\\s+");

                if (toks[0].equals("VERTEX2")) {
                    GXYTNode n = new GXYTNode();
                    n.state = new double[] { Double.parseDouble(toks[1]),
                                             Double.parseDouble(toks[2]),
                                             Double.parseDouble(toks[3]) };
                    n.init = LinAlg.copy(n.state);
                    g.nodes.add(n);
                } else if (toks[0].equals("EDGE2")) {
                    GXYTEdge e = new GXYTEdge();
                    e.nodes = new int[2];
                    e.nodes[0] = Integer.parseInt(toks[1]);
                    e.nodes[1] = Integer.parseInt(toks[2]);
                    e.z = new double[] { Double.parseDouble(toks[3]),
                                         Double.parseDouble(toks[4]),
                                         Double.parseDouble(toks[5]) };
                    e.P = new double[][] { { Double.parseDouble(toks[6]), Double.parseDouble(toks[7]), Double.parseDouble(toks[8]) },
                                           { Double.parseDouble(toks[7]), Double.parseDouble(toks[9]), Double.parseDouble(toks[10]) },
                                           { Double.parseDouble(toks[8]), Double.parseDouble(toks[10]), Double.parseDouble(toks[11]) } };
                    g.edges.add(e);
                } else {
                    System.out.println("???\n");
                }
            }

            g.write("/tmp/va.graph");
        } catch (IOException ex) {
            System.out.println("Ex: "+ex);
        }

    }

}
