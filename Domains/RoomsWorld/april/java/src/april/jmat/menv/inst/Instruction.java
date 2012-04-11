package april.jmat.menv.inst;

import java.util.*;

import april.jmat.menv.*;

public abstract class Instruction
{
    /** Instructions should not contain any temporary register
        indices, except in the in[] and out[] array below. Otherwise,
        the optimizer can generate bad code. **/
    public int in[];
    public int out[];

    public abstract void execute(Environment env);
}
