package april.jmat.menv;

import java.util.*;
import april.util.*;

public class Tokenizer
{
    static GenericTokenizer gtok;

    enum TYPE { WHITE, NUMBER, STRING, SYMBOL, OP, KEYWORD };

    static {
        gtok = new GenericTokenizer();

        // Don't enable this: if whitespace is enabled, then it will
        // greedily consume all white space, breaking the unary minus
        // below.
        //
        // gtok.add(TYPE.WHITE, "\\s+");

        // declare keywords first so they override symbols.
        gtok.addEscape(TYPE.KEYWORD, "for while if else function do return "+
                       "break continue");

        // we handle unary minus very carefully!
        // Consider:
        // [ 4-5 ]    that's the 1x1 matrix [-1]
        // [ 4 - 5 ]  that's the 1x1 matrix [-1]
        // [ 4 -5 ]   that's a 1x2 matrix.
        //
        // In other words, we consume a unary minus as part of the
        // number format IFF whitespace is explicitly there. Otherwise,
        // we will produce the minus sign separately.
        gtok.add(TYPE.NUMBER, "(\\s\\-)?[\\.0-9]+((e|E)\\-?[0-9]+)?");

        // this will catch stray MINUS signs that look like numbers at
        // first, but we'll also get an extra leading whitespace.
        gtok.add(TYPE.OP,"\\s\\-\\s");

        gtok.add(TYPE.STRING, "\"([^\"]|(\\\\.))*\"");
        gtok.add(TYPE.SYMBOL, "[_A-Za-z][_A-Za-z0-9]*");

        gtok.addEscape(TYPE.OP, "++ -- = <= == != >= += -= *= /= && || << >> " +
                       "<<= >>= <<< >>> .* ./ .^ + - / * \\ ' [ ] ( ) " +
                       "{ } ; < > , ^ !");

    }

    ArrayList<GenericTokenizer.Token<TYPE>> tokens;
    int pos;

    public Tokenizer(String s)
    {
        tokens = gtok.tokenize(s);
        for (int i = 0; i < tokens.size(); i++) {
            GenericTokenizer.Token<TYPE> token = tokens.get(i);

            if (token.type==TYPE.WHITE || token.type==null) {
                tokens.remove(i);
                i--;
            }

            // remove stray whitespace.
            token.token = token.token.trim();
        }
    }

    public String next()
    {
        GenericTokenizer.Token<TYPE> t = tokens.get(pos++);
        return t.token;
    }

    public String peek()
    {
        GenericTokenizer.Token<TYPE> t = tokens.get(pos);
        return t.token;
    }

    public TYPE peekType()
    {
        GenericTokenizer.Token<TYPE> t = tokens.get(pos);
        return t.type;
    }

    public boolean hasNext()
    {
        return pos < tokens.size();
    }

    public void unget()
    {
        pos--;
    }

    public boolean tryNext(String s)
    {
        if (!hasNext())
            return false;

        GenericTokenizer.Token<TYPE> t = tokens.get(pos);
        if (t.token.equals(s)) {
            pos++;
            return true;
        }

        return false;
    }

    // get a string centered around the current location.
    public String getNearString()
    {
        return null;
    }

    public void require(String s)
    {
        if (!hasNext())
            throw new MEnvRuntimeException("End of file encountered. Expected '"+s+"'");

        String tok = next();
        if (tok.equals(s))
            return;

        throw new MEnvRuntimeException("Expected '"+s+"' near '"+getNearString()+"'");
    }

    public static void main(String args[])
    {
        Tokenizer t = new Tokenizer(args[0]);
        Tokenizer.gtok.dumpNroot();

        while (t.hasNext())
            System.out.printf("%15s : %s\n", t.peekType(),t.next());
    }
}
