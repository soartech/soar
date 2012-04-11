package april.config;

import java.io.*;
import java.util.*;

import april.util.*;

class Tokenizer
{
    ArrayList<GenericTokenizer.Token<String>> tokens;
    int position;

    static GenericTokenizer<String> gt;

    static {
        gt = new GenericTokenizer<String>();

        gt.add("STRING", "\"[^\"]*\"");
        gt.addEscape("OP", "+{ { } = : , ; [ ]");
        gt.add("SYMBOL", "[a-zA-Z_\\.0-9\\-\\+#]+");

        gt.add(null, "#[^\n]*\\n"); // comment
        gt.add(null, "//[^\n]*\\n"); // comment
        gt.add(null, "\\s+");        // whitespace
    }

    public Tokenizer(File f) throws IOException
    {
        tokens = gt.tokenize(f);

        process();
    }

    public Tokenizer(String s)
    {
        tokens = gt.tokenize(s);

        process();
    }

    void process()
    {
        for (GenericTokenizer.Token<String> tok : tokens) {
            if (tok.type.equals("STRING")) {
                StringBuffer sb = new StringBuffer();
                for (int i = 1; i+1 < tok.token.length(); i++) {
                    char c = tok.token.charAt(i);
                    if (c=='\\' && i+2 < tok.token.length()) {
                        i++;
                        c = tok.token.charAt(i);
                        switch (c) {
                            case 'n':
                                c = '\n';
                                break;
                            case 'r':
                                c = '\r';
                                break;
                            case 't':
                                c = '\t';
                                break;
                            default:
                        }
                    }
                    sb.append(c);
                }
                tok.token = sb.toString();
            }
        }
    }

    public boolean hasNext() throws IOException
    {
        return position < tokens.size();
    }

    // If the next token is s, consume it.
    public boolean consume(String s) throws IOException
    {
        if (tokens.get(position).token.equals(s)) {
            position++;
            return true;
        }

        return false;
    }

    public String peek() throws IOException
    {
        if (!hasNext())
            return null;

        return tokens.get(position).token;
    }

    public String next() throws IOException
    {
        if (!hasNext())
            return null;

        GenericTokenizer.Token<String> t = tokens.get(position++);

        return t.token;
    }

    public static void main(String args[])
    {
        try {
            Tokenizer t = new Tokenizer(new File(args[0]));
            while (t.hasNext()) {
                System.out.println(t.next());
            }

        } catch (IOException ex) {
            System.out.println("ex: "+ex);
        }
    }
}
