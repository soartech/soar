package april.jmat.menv;

import java.util.*;

import april.jmat.menv.inst.*;
import april.jmat.menv.node.*;

public class Emit
{
    ArrayList<Instruction> instructions = new ArrayList<Instruction>();

    // these two stacks are used to implement break and continue:
    // when entering a new scope that supports break/continue, push
    // the appropriate label ids.
    Stack<Integer> breakStack = new Stack<Integer>();
    Stack<Integer> continueStack = new Stack<Integer>();

    int nextLabel = 1;
    int nextVar = 1;

    ////////////////////////////////////////////////////////////////

    public Emit(Node n)
    {
        n.emit(this);
    }

    public void emit(Instruction inst)
    {
        instructions.add(inst);
    }

    public int allocVar()
    {
        return nextVar++;
    }

    public int allocLabel()
    {
        return nextLabel++;
    }

    public void emitLabel(int label)
    {
        emit(new LabelInstruction(label));
    }

    public int peekBreakLabel()
    {
        return breakStack.peek();
    }

    public void pushBreakLabel(int label)
    {
        breakStack.push(label);
    }

    public void popBreakLabel()
    {
        breakStack.pop();
    }

    public int peekContinueLabel()
    {
        return continueStack.peek();
    }

    public void pushContinueLabel(int label)
    {
        continueStack.push(label);
    }

    public void popContinueLabel()
    {
        continueStack.pop();
    }

    public void optimize()
    {
    }

    void deadCodeElimination()
    {
        HashSet<Integer> used = new HashSet<Integer>();

        for (Instruction inst : instructions) {
        }
    }
}
