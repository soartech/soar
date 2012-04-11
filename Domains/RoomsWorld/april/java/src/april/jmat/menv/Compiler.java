package april.jmat.menv;

/*
  dim:
  expr COLON expr

  subsel: LBRACK dim (COMMA dim)? RBRACK

  term:
  LBRACK exprs... RBRACK                     // matrix literal
  LPAREN expr RPAREN                         // parenthetical expr
  UNARYOP term                               // unary minus, negate
  number                                     // literal
  "quotedstring"
  symbol LPAREN exprs... RPAREN              // function call
  symbol LBRACK exprs... RBRACK              // subselect
  symbol                                     // variable

  tterm:
  term TRANS
  term ++
  term --
  term

  expr:
  tterm (OP tterm)*                          // expression chain

  stmt:
  LBRACE stmt* RBRACE                        // block
  if LPAREN expr RPAREN stmt (else stmt)?    // if statement
  while LPAREN expr RPAREN stmt              // while loop
  for LPAREN expr SEMI expr SEMI stmt        // for loop
  SEMI                                       // empty stmt
  expr (EQUAL expr)?                         // eval or assign
  return expr
  break
  continue


  input:  // top-level user input
  function symbol LPAREN ... RPAREN ...      // function declaration
  stmt


  TODO: need to support +=, -=, etc.
*/

import april.jmat.menv.inst.*;
import april.jmat.menv.node.*;

import java.util.*;

import april.jmat.*;

public class Compiler
{
    public ArrayList<Assembly> commands  = new ArrayList<Assembly>();
    public ArrayList<Function> functions = new ArrayList<Function>();

    static ConstNode ONE = new ConstNode(1.0);

    public Compiler(String input)
    {
        compile(input);
    }

    Node parseTerm(Tokenizer t)
    {
        if (!t.hasNext())
            return null;

        if (t.tryNext("[")) {
            // matrix literal

            ArrayList<ArrayList<Node>> rows = new ArrayList<ArrayList<Node>>();
            boolean gotend = false;

            while (!gotend) {
                ArrayList<Node> row = new ArrayList<Node>();

                while (true) {
                    t.tryNext(",");
                    if (t.tryNext(";"))
                        break;
                    if (t.tryNext("]")) {
                        gotend = true;
                        break;
                    }

                    Node n = parseExpression(t);
                    if (n == null)
                        throw new MEnvRuntimeException("Invalid matrix literal near '"+t.getNearString()+"'");

                    row.add(n);
                }
                rows.add(row);
            }

            return new ConcatNode(rows);
        }

        if (t.tryNext("(")) {
            Node n = parseExpression(t);
            t.require(")");
            return n;
        }

        if (t.tryNext("+")) {
            // useless unary plus-- just drop it.
            return parseTerm(t);
        }

        // constant literal?
        if (t.peekType()==Tokenizer.TYPE.NUMBER) {
            try {
                return new ConstNode(Double.parseDouble(t.next()));
            } catch (NumberFormatException ex) {
                throw new MEnvRuntimeException("Invalid number format");
            }
        }

        // constant string?
        if (t.peekType()==Tokenizer.TYPE.STRING) {
            String s = t.next();
            // remove quotes.
            s = s.substring(1, s.length() - 1);
            // must unescape string and removing enclosing quotes.
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < s.length(); i++) {
                char c = s.charAt(i);
                if (c=='\\') {
                    i++; // consume.
                    c = s.charAt(i);
                    if (c=='n')
                        sb.append('\n');
                    else if (c=='t')
                        sb.append('\t');
                    else
                        sb.append(c);
                } else {
                    sb.append(c);
                }
            }
            return new ConstNode(sb.toString());
        }

        String tok = t.next();

        if (tok.equals("-") || tok.equals("!") || tok.equals("~")) {
            // unary minus
            return new OpNode(tok, null, parseTerm(t));
        }

        if (!isSymbol(tok)) {
            t.unget();
            return null;
        }

        if (t.tryNext("(")) {
            // function call
            ArrayList<Node> parameters = new ArrayList<Node>();

            while (!t.tryNext(")")) {
                Node n = parseExpression(t);
                if (n==null)
                    throw new MEnvRuntimeException("Invalid function call near '"+t.getNearString()+"'");
                parameters.add(n);
                t.tryNext(",");
            }

            return new CallNode(tok, parameters);
        }

        // just a variable reference.
        return new VarNode(tok);
    }

    Node parseTransTerm(Tokenizer t)
    {

        if (t.tryNext("++")) {

            Node n = parseTerm(t);

            OpNode nplus = new OpNode("+", n, ONE);
            AssignNode an = new AssignNode(n, nplus);

            return an;
        }

        if (t.tryNext("--")) {

            Node n = parseTerm(t);

            OpNode nplus = new OpNode("-", parseTerm(t), ONE);
            AssignNode an = new AssignNode(n, nplus);

            return an;
        }

        Node n = parseTerm(t);

        if (t.tryNext("[")) {
            Node ra = null, rb = null, ca = null, cb = null;

            // subselects
            ra = parseExpression(t);
            if (t.tryNext(":"))
                rb = parseExpression(t);

            if (t.tryNext(",")) {
                ca = parseExpression(t);
                if (t.tryNext(":"))
                    cb = parseExpression(t);
            }

            t.require("]");
            return new SubselNode(n, ra, rb, ca, cb);
        }

        if (t.tryNext("'")) {
            do {
                n = new OpNode("'", null, n);
            } while (t.tryNext("'"));

            return n;
        }

        // postfix operators: the value of these expressions
        // reflects the original value of the variable.
        // This is a pain to do cleanly, so we just fix up
        // the return value manually. Dead-code elimination
        // should eliminate the computational inefficiency.
        if (t.tryNext("++")) {

            OpNode nplus = new OpNode("+", n, ONE);
            AssignNode an = new AssignNode(n, nplus);

            return new OpNode("-", an, ONE);
        }

        if (t.tryNext("--")) {

            OpNode nplus = new OpNode("-", n, ONE);
            AssignNode an = new AssignNode(n, nplus);

            return new OpNode("+", an, ONE);
        }

        return n;
    }

    Node parseExpression(Tokenizer t)
    {
        ArrayList<Node> nodes = new ArrayList<Node>();
        ArrayList<OpNode> ops = new ArrayList<OpNode>();

        if (!t.hasNext())
            return null;

        // consume the first term
        Node n0 = parseTransTerm(t);
        nodes.add(n0);
        if (!t.hasNext())
            return n0;

        // consume a stretch of OP, Node pairs.
        while (true) {
            if (!t.hasNext())
                break;

            String tok = t.next();
            if (OpNode.OP.fromToken(tok) == null) {
                t.unget();
                break;
            }

            OpNode op = new OpNode(tok, null, null);
            ops.add(op);

            Node n = parseTransTerm(t);
            nodes.add(n);
        }

        assert(nodes.size()==ops.size()+1);

        // XXX: This is really inefficient, but in general there's not
        // much to do.
        while (ops.size() > 0) {
            // consider all ops until there are no ops left
            for (int i = 0; i < ops.size(); i++) {
                OpNode op = ops.get(i);

                // we can process this op if its precedence is high enough
                int prec = op.getPrecedence();
                int prevprec = i-1 >=0 ? ops.get(i-1).getPrecedence() : -10000;
                int nextprec = i+1 < ops.size() ? ops.get(i+1).getPrecedence() : -10000;

                if (prec >= nextprec && prec > prevprec) {
                    // get the lhs and rhs
                    op.lhs = nodes.get(i);
                    op.rhs = nodes.get(i+1);

                    // replace the lhs and rhs with a single node (our result)
                    nodes.remove(i+1);
                    nodes.set(i, op);

                    // we're done with this node.
                    ops.remove(i);
                }
            }
        }

        assert(nodes.size()==1);
        return nodes.get(0);
    }

    BlockNode parseBlock(Tokenizer t)
    {
        BlockNode block = new BlockNode();

        Node n;
        while ((n = parseStatement(t)) != null)
            block.statements.add(n);

        return block;
    }

    boolean isSymbol(String s)
    {
        char c = s.charAt(0);
        return Character.isLetter(c) || c=='_';
    }

    /** Parse a statement, either contained in curly braces or
        terminating in a semicolon. In many cases, if there is no
        semi-colon, we will parse successfully anyway. **/
    Node parseStatement(Tokenizer t)
    {
        if (!t.hasNext())
            return null;

        if (t.tryNext("{")) {
            Node n = parseBlock(t);
            t.require("}");
            return n;
        }

        if (t.tryNext("if")) {
            t.require("(");
            IfNode n = new IfNode();
            n.cond = parseExpression(t);
            t.require(")");
            n.trueClause = parseStatement(t);
            if (t.tryNext("else"))
                n.falseClause = parseStatement(t);
            return n;
        }

        if (t.tryNext("while")) {
            t.require("(");
            WhileNode n = new WhileNode();
            n.cond = parseExpression(t);
            t.require(")");
            n.body = parseStatement(t);
            return n;
        }

        if (t.tryNext("for")) {
            t.require("(");
            ForNode n = new ForNode();
            n.init = parseStatement(t);
            // semicolons are consumed by parseStatement!
            //	    t.tryNext(";");
            n.cond = parseStatement(t);
            //	    t.tryNext(";");
            n.incr = parseStatement(t);
            t.require(")");
            n.body = parseStatement(t);
            return n;
        }

        if (t.tryNext("return")) {
            ReturnNode n = new ReturnNode(parseExpression(t));
            t.tryNext(";");
            return n;
        }

        if (t.tryNext("break")) {
            t.tryNext(";");
            return new BreakNode();
        }

        if (t.tryNext("continue")) {
            t.tryNext(";");
            return new ContinueNode();
        }

        if (t.tryNext(";")) {
            return new NopNode();
        }

        // eval or assignment
        Node lhs = parseExpression(t);
        if (lhs == null)
            return null;

        if (t.tryNext("=")) {
            AssignNode an = new AssignNode(lhs, parseExpression(t));

            t.tryNext(";");
            return an;
        }

        if (t.tryNext("+=")) {
            AssignNode an = new AssignNode(lhs, new OpNode("+", lhs, parseExpression(t)));
            t.tryNext(";");
            return an;
        }

        if (t.tryNext("-=")) {
            AssignNode an = new AssignNode(lhs, new OpNode("-", lhs, parseExpression(t)));
            t.tryNext(";");
            return an;
        }

        if (t.tryNext("*=")) {
            AssignNode an = new AssignNode(lhs, new OpNode("*", lhs, parseExpression(t)));
            t.tryNext(";");
            return an;
        }

        if (t.tryNext("/=")) {
            AssignNode an = new AssignNode(lhs, new OpNode("/", lhs, parseExpression(t)));
            t.tryNext(";");
            return an;
        }

        t.tryNext(";");
        return lhs;
    }

    void compile(String expr)
    {
        Tokenizer t = new Tokenizer(expr);

        while (t.hasNext()) {

            if (t.tryNext("function")) {
                String functionName = t.next();
                ArrayList<String> parameterNames = new ArrayList<String>();
                ArrayList<Object> parameterDefaults = new ArrayList<Object>();

                t.require("(");
                while (!t.tryNext(")")) {
                    parameterNames.add(t.next());
                    if (t.tryNext("=")) {
                        Node n = parseExpression(t);
                        if (!(n instanceof ConstNode))
                            throw new MEnvRuntimeException("default arguments must be constant");

                        parameterDefaults.add(((ConstNode) n).v);
                    } else {
                        parameterDefaults.add(null);
                    }
                    t.tryNext(",");
                }

                t.require("{");
                Node body = parseBlock(t);
                t.require("}");

                Emit e = new Emit(body);
                ArrayList<Instruction> insts = e.instructions;
                insts = Optimizer.optimize(insts);
                Assembly asm = new Assembly(insts);

                functions.add(new Function(functionName, parameterNames, parameterDefaults, asm));
                continue;
            }

            Node stmt = parseBlock(t);
            if (stmt != null) {

                // if we didn't parse anything but there's still input, then there's
                // something weird in the token stream.
                if (((BlockNode) stmt).statements.size()==0 && t.hasNext())
                    throw new MEnvRuntimeException("Unexpected input '"+t.peek()+"' near '"+t.getNearString()+"'");

                // wrap the statement in a return so that we'll
                // give the answer to the user.
                stmt = new ReturnNode(stmt);

                Emit e = new Emit(stmt);
                ArrayList<Instruction> insts = e.instructions;
                insts = Optimizer.optimize(insts);
                Assembly asm = new Assembly(insts);

                commands.add(asm);
            }
        }
    }
}
