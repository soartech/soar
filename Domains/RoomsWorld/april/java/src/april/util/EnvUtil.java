package april.util;

public class EnvUtil
{
    public static final boolean getProperty(String name, boolean def)
    {
        String s = System.getProperty(name);
        if (s==null)
            return def;

        s = s.trim().toLowerCase();
        if (s.equals("0") || s.equals("false") || s.equals("no"))
            return false;
        if (s.equals("1") || s.equals("true") || s.equals("yes"))
            return true;
        System.out.println(name+": Bad value "+s);
        return def;
    }

    public static final int getProperty(String name, int def)
    {
        String s = System.getProperty(name);
        if (s==null)
            return def;

        try {
            return Integer.parseInt(s);
        } catch (Exception ex) {
        }

        System.out.println(name+": Bad value "+s);
        return def;
    }

    // find
    public static String expandVariables(String in)
    {
        StringBuffer sb = new StringBuffer();

        int inpos = 0;
        while (inpos < in.length()) {

            char c = in.charAt(inpos);

            if (c != '$') {
                sb.append(c);
                inpos++;
                continue;
            }

            // consume $
            inpos++;

            // we've found an env. variable.
            StringBuffer varname = new StringBuffer();
            while (inpos < in.length() && isVariableCharacter(in.charAt(inpos))) {
                varname.append(in.charAt(inpos));
                inpos++;
            }

            sb.append(System.getenv(varname.toString()));
        }

        return sb.toString();
    }

    // is the character permissible in a Bash environment variable name?
    static final boolean isVariableCharacter(char c)
    {
        return (Character.isLetterOrDigit(c) || c=='_');
    }
}
