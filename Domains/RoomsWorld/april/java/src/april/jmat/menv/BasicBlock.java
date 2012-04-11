package april.jmat.menv;

import java.util.*;

import april.jmat.menv.inst.*;

// These optimizations require that the instructions not yet be
// assembled, since the values of labels will change as instructions
// are removed.
public class BasicBlock
{
    public ArrayList<Instruction> instructions = new ArrayList<Instruction>();

    // split instruction stream into a bunch of basic blocks with the
    // property that labels and branches always appear in blocks by
    // themselves.
    public static ArrayList<BasicBlock> toBasicBlocks(ArrayList<Instruction> insts)
    {
        ArrayList<BasicBlock> bblocks = new ArrayList<BasicBlock>();

        BasicBlock thisblock = new BasicBlock();

        for (Instruction inst : insts) {
            if (inst instanceof LabelInstruction ||
                inst instanceof BranchInstruction) {

                if (thisblock.instructions.size()>0) {
                    bblocks.add(thisblock);
                    thisblock = new BasicBlock();
                }

                thisblock.instructions.add(inst);
                bblocks.add(thisblock);
                thisblock = new BasicBlock();
                continue;
            }

            thisblock.instructions.add(inst);
        }

        if (thisblock.instructions.size()>0)
            bblocks.add(thisblock);

        return bblocks;
    }

    // re-concatenate the basic blocks.
    public static ArrayList<Instruction> toInstructions(ArrayList<BasicBlock> bblocks)
    {
        ArrayList<Instruction> instructions = new ArrayList<Instruction>();

        for (BasicBlock bblock : bblocks) {
            for (Instruction inst : bblock.instructions)
                instructions.add(inst);
        }

        return instructions;
    }

    // We've determined that register 'victim' can be replaced by
    // 'replacement'. Consequently, change all usages of 'victim' with
    // 'replacement'. (We only modify the inputs, thus making the instruction
    // that produced 'victim' removable via dead-code elimination.)
    public void replaceRegister(int victim, int replacement)
    {
        // only replace input registers-- the instruction that writes
        // the victim will be removed via dead code elimination.
        for (Instruction inst : instructions) {
            if (inst.in != null)
                for (int i = 0; i < inst.in.length; i++)
                    if (inst.in[i]==victim)
                        inst.in[i] = replacement;
        }
    }

    // Use a previously stored register rather than re-loading a
    // variable. We take some care to "invalidate" a register after it
    // could become invalid (after a store).
    public void optimizeRedundantLoads()
    {
        HashMap<String, Integer> varMap = new HashMap<String,Integer>();

        for (int i = 0; i < instructions.size(); i++) {
            Instruction inst = instructions.get(i);

            // invalidate the last known register
            if (inst instanceof SubselStoreInstruction)
                varMap.put(((SubselStoreInstruction) inst).name, null);

            if (inst instanceof StoreInstruction) {
                StoreInstruction sinst = (StoreInstruction) inst;
                varMap.put(sinst.name, sinst.in[0]);
            }

            if (inst instanceof LoadInstruction) {
                LoadInstruction linst = (LoadInstruction) inst;
                Integer r = varMap.get(linst.name);
                if (r!=null) {
                    replaceRegister(linst.out[0], r);
                    //		    System.out.printf("replace %d %d\n", linst.out[0], r);
                } else {
                    varMap.put(linst.name, linst.out[0]);
                }
            }
        }
    }

    // Make usages of the same constant use the same register.
    public void optimizeRedundantConsts()
    {
        ArrayList<Integer> temps = new ArrayList<Integer>();
        ArrayList<Object> values = new ArrayList<Object>();

        for (int i = 0; i < instructions.size(); i++) {
            Instruction inst = instructions.get(i);

            if (inst instanceof ConstInstruction) {
                ConstInstruction cinst = (ConstInstruction) inst;
                Object Mi = cinst.v;

                boolean found = false;

                for (int j = 0; j < values.size(); j++) {
                    Object Mj = values.get(j);

                    if (TypeUtil.equal(Mi, Mj)) {
                        replaceRegister(cinst.out[0], temps.get(j));
                        found = true;
                        break;
                    }
                }

                if (!found) {
                    temps.add(cinst.out[0]);
                    values.add(cinst.v);
                }
            }
        }
    }

    // Eliminate duplicate function calls
    public void commonCallElimination()
    {

        for (int i = 0; i < instructions.size(); i++) {

            if (!(instructions.get(i) instanceof CallInstruction))
                continue;

            CallInstruction insti = (CallInstruction) instructions.get(i);
            Function f = BuiltinFunctions.functions.get(insti.functionName);
            if (f == null || !f.sideEffectFree)
                continue;

            // same as any of the preceeding instructions?
            for (int j = 0; j < i; j++) {

                if (!(instructions.get(j) instanceof CallInstruction))
                    continue;

                CallInstruction instj = (CallInstruction) instructions.get(j);

                if (insti.in.length != instj.in.length)
                    continue;

                if (!insti.functionName.equals(instj.functionName))
                    continue;

                boolean match = true;
                for (int k = 0; k < insti.in.length; k++)
                    if (insti.in[k] != instj.in[k])
                        match = false;

                if (match)
                    replaceRegister(insti.out[0], instj.out[0]);

                System.out.println("CCE "+insti+" "+instj+" "+match);
            }
        }
    }

    // Eliminate duplicate OpInstructions.
    public void commonSubexpressionElimination()
    {
        for (int i = 0; i < instructions.size(); i++) {

            if (!(instructions.get(i) instanceof OpInstruction))
                continue;

            OpInstruction insti = (OpInstruction) instructions.get(i);

            // same as any of the preceeding instructions?
            for (int j = 0; j < i; j++) {

                if (!(instructions.get(j) instanceof OpInstruction))
                    continue;

                OpInstruction instj = (OpInstruction) instructions.get(j);

                if (insti.in.length != instj.in.length)
                    continue;

                if (insti.op != instj.op)
                    continue;

                boolean match = true;
                for (int k = 0; k < insti.in.length; k++)
                    if (insti.in[k] != instj.in[k])
                        match = false;

                if (match)
                    replaceRegister(insti.out[0], instj.out[0]);
            }
        }
    }
}
