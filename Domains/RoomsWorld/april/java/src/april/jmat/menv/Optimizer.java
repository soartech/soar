package april.jmat.menv;

import java.util.*;

import april.jmat.menv.inst.*;
import april.jmat.*;
import april.util.*;

public class Optimizer
{
    public static boolean verbose = false;
    public static boolean enable = true;

    public static ArrayList<Instruction> optimize(ArrayList<Instruction> instructions)
    {
        if (!enable)
            return instructions;

        if (verbose) {
            System.out.println("--------------------------------------");
            System.out.println("Before optimization:");
            new Assembly(instructions).dump();
        }

        Tic tic = new Tic();

        while (true) {

            int sizeBefore = instructions.size();

            deadFlowControlElimination(instructions);

            ArrayList<BasicBlock> bblocks = BasicBlock.toBasicBlocks(instructions);

            for (BasicBlock b : bblocks) {
                b.optimizeRedundantLoads();
                b.optimizeRedundantConsts();
                b.commonSubexpressionElimination(); // should follow redundant load/consts
                b.commonCallElimination();
            }

            instructions = BasicBlock.toInstructions(bblocks);

            deadCodeElimination(instructions);
            renumberRegisters(instructions);

            // if this optimization pass didn't accomplish anything,
            // then we're done. Otherwise, we'll do another pass.
            if (instructions.size() == sizeBefore)
                break;
        }

        if (verbose) {
            double elapsedTime = tic.toc();
            System.out.println("\nAfter optimization");
            new Assembly(instructions).dump();

            System.out.printf("\noptimization time: %.3f ms\n",elapsedTime*1000);
        }

        return instructions;
    }

    //////////////////////////////////////////////////////////////////////
    // search for flow-control instructions (unconditional branches,
    // returns) that could not possibly be reached (because there is
    // no label between them and a previous flow-control instruction.)
    //
    // Then, find labels that have no branches to them, and remove
    // them.
    public static void deadFlowControlElimination(ArrayList<Instruction> instructions)
    {
        boolean live = true;

        // which labels have a live branch pointing to them?
        HashSet<Integer> liveLabels = new HashSet<Integer>();

        for (int i = 0; i < instructions.size(); i++) {
            Instruction inst = instructions.get(i);

            if (inst instanceof LabelInstruction) {
                live = true;
                continue;
            }

            if (!live) {
                instructions.remove(i);
                i--;
                continue;
            }

            if (inst instanceof ReturnInstruction)
                live = false;

            if (inst instanceof BranchInstruction) {
                BranchInstruction binst = (BranchInstruction) inst;
                if (binst.in == null)
                    live = false;

                liveLabels.add(binst.label);
            }
        }

        // search for (and remove) dead labels
        for (int i = 0; i < instructions.size(); i++) {
            Instruction inst = instructions.get(i);

            if (!(inst instanceof LabelInstruction))
                continue;

            LabelInstruction linst = (LabelInstruction) inst;
            if (!liveLabels.contains(linst.label)) {
                // it's dead. remove it.
                instructions.remove(i);
                i--;
            }
        }
    }

    //////////////////////////////////////////////////////////////////////

    public static void deadCodeElimination(ArrayList<Instruction> instructions)
    {
        // compute a set of live registers: those that write to
        // variables, are part of a return value, or are used to call
        // another function.

        // initialize by looking at stores and returns.

        HashSet<Integer> live = new HashSet();
        for (Instruction inst : instructions) {
            if (inst instanceof ReturnInstruction) {
                ReturnInstruction rinst = (ReturnInstruction) inst;
                if (rinst.in != null)
                    live.add(rinst.in[0]);
            }

            if (inst instanceof StoreInstruction) {
                StoreInstruction sinst = (StoreInstruction) inst;
                live.add(sinst.in[0]);
            }

            if (inst instanceof SubselStoreInstruction) {
                SubselStoreInstruction sinst = (SubselStoreInstruction) inst;
                for (int i = 0; i < sinst.in.length; i++)
                    live.add(sinst.in[i]);
            }

            if (inst instanceof BranchInstruction) {
                BranchInstruction binst = (BranchInstruction) inst;
                for (int i = 0; binst.in!=null && i < binst.in.length; i++)
                    live.add(binst.in[i]);
            }

            if (inst instanceof CallInstruction) {
                CallInstruction cinst = (CallInstruction) inst;
                Function f = BuiltinFunctions.functions.get(cinst.functionName);
                // functions that are side-effect-free are not automatically live.
                if (f==null || !f.sideEffectFree)
                    for (int i = 0; cinst.in!=null && i < cinst.in.length; i++)
                        live.add(cinst.in[i]);
            }
        }

        // find any values used to compute the live registers,
        // and keep iterating until the set stops growing.
        while (true) {
            int sizeBefore = live.size();

            for (int idx = instructions.size()-1; idx >=0; idx--) {
                Instruction inst = instructions.get(idx);

                if (inst.out == null)
                    continue;

                if (inst.out!=null) {
                    for (int i = 0; i < inst.out.length; i++) {
                        if (live.contains(inst.out[i]) && inst.in!=null) {
                            for (int j = 0; j < inst.in.length; j++) {
                                live.add(inst.in[j]);
                            }
                            break;
                        }
                    }
                }
            }

            // no changes. we're done.
            if (sizeBefore == live.size())
                break;
        }

        if (false && verbose) {
            System.out.println("live: ");
            for (Integer i : live) {
                System.out.printf("r%d ", i);
            }
            System.out.println("");
        }

        // now, remove dead instructions.
        for (int i = 0; i < instructions.size(); i++) {
            Instruction inst = instructions.get(i);

            // skip never-dead instructions.
            if (inst instanceof StoreInstruction ||
                inst instanceof SubselStoreInstruction ||
                inst instanceof ReturnInstruction ||
                inst instanceof BranchInstruction ||
                inst instanceof LabelInstruction)
                continue;

            if (inst instanceof CallInstruction) {
                CallInstruction cinst = (CallInstruction) inst;
                Function f = BuiltinFunctions.functions.get(cinst.functionName);

                // if function might not be side-effect free, then we
                // can't remove it.
                if (f==null || !f.sideEffectFree)
                    continue;
            }

            boolean thisLive = false;
            for (int j = 0; inst.out!=null && j < inst.out.length; j++) {
                if (live.contains(inst.out[j])) {
                    thisLive = true;
                    break;
                }
            }

            // if it's dead, delete it.
            if (!thisLive) {
                instructions.remove(i);
                i--;
            }
        }
    }

    public static void renumberRegisters(ArrayList<Instruction> instructions)
    {
        HashMap<Integer,Integer> map = new HashMap<Integer,Integer>();

        int nextReg = 0;

        for (Instruction inst : instructions) {
            for (int i = 0; inst.in!=null && i < inst.in.length; i++) {
                int from = inst.in[i];
                if (from < 0) // don't renumber these-- they represent flags.
                    continue;
                int to;
                if (map.containsKey(from)) {
                    to = map.get(from);
                } else {
                    to = nextReg++;
                    map.put(from, to);
                }

                inst.in[i] = to;
            }

            for (int i = 0; inst.out!=null && i < inst.out.length; i++) {
                int from = inst.out[i];
                if (from < 0)
                    continue;
                int to;
                if (map.containsKey(from)) {
                    to = map.get(from);
                } else {
                    to = nextReg++;
                    map.put(from, to);
                }

                inst.out[i] = to;
            }
        }
    }
}
