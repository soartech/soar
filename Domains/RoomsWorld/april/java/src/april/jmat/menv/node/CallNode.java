package april.jmat.menv.node;

import java.util.*;

import april.jmat.menv.*;
import april.jmat.menv.inst.*;

public class CallNode implements Node
{
    public CallNode(String function, ArrayList<Node> parameters)
    {
        this.function = function;
        this.parameters = parameters;
    }

    ArrayList<Node> parameters = new ArrayList<Node>();
    String function;
    /*
      public Matrix eval(MEnv env)
      {
      Function f = env.getFunction(function);
      if (f==null)
      throw new MEnvRuntimeException("Unknown function '"+function+"'");

      MEnv childEnv = new MEnv();
      childEnv.funcs = env.funcs;
      for (int i = 0; i < f.parameterNames.size(); i++) {
      Node n = i < parameters.size() ? parameters.get(i) : f.parameterDefaults.get(i);
      childEnv.vars.put(f.parameterNames.get(i), n.eval(env));
      }

      return f.eval(childEnv);
      }
    */

    public String toString()
    {
        StringBuffer sb = new StringBuffer();
        sb.append(String.format("(function %s ", function));
        for (Node n : parameters)
            sb.append(String.format(n.toString()+" "));

        sb.append(")");
        return sb.toString();
    }

    public int emit(Emit e)
    {
        int r = e.allocVar();

        ArrayList<Integer> params = new ArrayList<Integer>();

        for (int i = 0; i < parameters.size(); i++) {
            Node n = parameters.get(i);
            params.add(n.emit(e));
        }

        e.emit(new CallInstruction(r, function, params));

        return r;
    }
}
