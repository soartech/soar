package april.jmat.menv;

import java.util.*;

import april.jmat.menv.inst.*;

public class Assembly
{
    public ArrayList<Instruction> instructions;
    public int    rMax;

    public Assembly(ArrayList<Instruction> input)
    {
        instructions = new ArrayList<Instruction>();
        HashMap<Integer, Integer> labelToIndex = new HashMap<Integer, Integer>();

        // pass 1: remove labels, fill in labelToIndex table
        for (int i = 0; i < input.size(); i++) {
            Instruction inst = input.get(i);
            if (inst instanceof LabelInstruction)
                labelToIndex.put(((LabelInstruction) inst).label, instructions.size());
            else
                instructions.add(inst);
        }

        // pass 2: resolve labels
        // resolve branches
        rMax = 0;
        for (Instruction inst : instructions) {
            if (inst instanceof BranchInstruction) {
                BranchInstruction binst = (BranchInstruction) inst;
                binst.labelidx = labelToIndex.get(binst.label);
            }

            if (inst.out != null)
                for (int i = 0; i < inst.out.length; i++)
                    rMax = Math.max(rMax, inst.out[i]);

            if (inst.in != null)
                for (int i = 0; i < inst.in.length; i++)
                    rMax = Math.max(rMax, inst.in[i]);
        }
    }


    public void dump()
    {
        for (int i = 0; i < instructions.size(); i++) {
            Instruction inst = instructions.get(i);

            System.out.printf("%5d : %s\n", i, inst);
        }
    }

}
