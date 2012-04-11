package april.config;

import java.io.*;
import java.util.*;

import april.util.*;

/** Concrete implementation of Config using a file. **/
public class ConfigFile extends Config
{
    public ConfigFile(String path) throws IOException
    {
        this(new File(path));
    }

    public ConfigFile(File f) throws IOException
    {
        prefix = "";
        basePath = f.getParent() + File.separator;
        merge(f);
    }

    public void merge(File f) throws IOException
    {
        Tokenizer t = new Tokenizer(f);

        parse(t, "", 0);
    }

    void parseError(Tokenizer t, String msg)
    {
        System.out.println("Parse error: "+msg);
//        System.out.println("Near line "+t.lineNumber+": "+t.line);
    }

    void copyProperties(String srcNameSpace, String destNameSpace)
    {
        HashMap<String, String[]> newkeys = new HashMap<String, String[]>();

        //Inelegant way to combine two Sets...
        Set<String> cKeySet = keys.keySet();
        Set<String> aKeySet = abstractKeys.keySet();
        ArrayList<String> keyList = new ArrayList<String>(aKeySet.size() + cKeySet.size());
        for(String aKey : aKeySet) keyList.add(aKey);
        for(String cKey : cKeySet) keyList.add(cKey);

        //Search in the concrete and abstarct keys together. Watch out for ':' beginning
        for (String key : keyList) {
            if (key.startsWith(srcNameSpace))
            {
                String propertyName = key.substring(srcNameSpace.length());
                String newKeyName = destNameSpace + propertyName;

                if (destNameSpace.startsWith(":")) {
                    newKeyName = ":" + newKeyName.replace(":","");
                } else {
                    newKeyName = newKeyName.replace(":","");
                }

                if (key.startsWith(":")) {
                        newkeys.put(newKeyName, abstractKeys.get(key));
                } else {
                    newkeys.put(newKeyName, keys.get(key));
                }
            }
        }

        for (String key : newkeys.keySet()) {
            if (destNameSpace.startsWith(":")) {
                abstractKeys.put(key, newkeys.get(key));
            } else {
                keys.put(key, newkeys.get(key));
            }
        }
    }

    /**
     * @param keyroot The namespace of the key (i.e., everything
     * except the property), well formed so that keyroot+propertyname
     * has dots in the right spots.
     **/
    void parse(Tokenizer t, String keyroot, int depth) throws IOException
    {
        int instantiateId = 0;

        while (true) {

            if (!t.hasNext())
                return;

/*
// anonymous closures are useless and so disabled
            if (t.consume("{")) {
                // a non-inheriting anonymous closure.
                // an anonymous enclosure? Anonymous enclosures are
                // used for scoping inheritance without introducing
                // extra namespace segments into the path.
                parse(t, keyroot, depth+1);
                continue;
            }
*/

            // end of block?
            if (t.consume("}")) {
                if (depth == 0)
                    parseError(t, "Unmatched } in input");

                return;
            }

            if (!t.hasNext())
                return;

            String keypart = null;

            // parse a key block.
            if (t.consume(":")) {
                keypart = ":" + t.next();
            } else {
                keypart = t.next();
            }

            if (keypart.endsWith("#")) {
                // System.out.println("*********: "+keypart);
                keypart = keypart.substring(0, keypart.length()-1) + instantiateId;
                instantiateId++;
            }

            if (!t.hasNext()) {
                parseError(t, "Premature EOF");
                return;
            }

            // inheriting?
            if (t.consume(":")) {
                while (true) {
                    String superclass = t.next();
                    copyProperties(":"+superclass+".", keyroot+keypart+".");
                    copyProperties(superclass+".", keyroot+keypart+".");

                    if (!t.consume(","))
                        break;
                }
            }

            // we have a non-inheriting enclosure block?
            if (t.consume("{")) {
                parse(t, keyroot+keypart+".", depth + 1);
                continue;
            }

            if (t.consume("+{")) {
                copyProperties(keyroot, keyroot+keypart+".");
                parse(t, keyroot+keypart+".", depth + 1);
                continue;
            }

            // This is a key/value declaration.
            // keypart is the key.

            String tok = t.next();
            if (!tok.equals("=")) {
                parseError(t, "Expected = got "+tok);
                return;
            }

            ArrayList<String> values = new ArrayList<String>();

            if (t.consume("[")) {
                // read a list of values
                while (true) {
                    tok = t.next();
                    if (tok.equals("]"))
                        break;
                    values.add(tok);
                    tok = t.peek();
                    if (tok.equals(","))
                        t.next();
                }
            } else {
                // read a single value
                values.add(t.next());
            }

            if (!t.consume(";"))
                parseError(t, "Expected ; got "+tok);

            String key = keyroot+keypart;

            if (keys.get(key)!=null) {
//                parseError(t, "Duplicate key definition for: "+key);
            }

            //Check for 'abstract' or 'invisible' keys
            if (key.startsWith(":")){
                abstractKeys.put(key, values.toArray(new String[0]));
            }else{
                keys.put(key, values.toArray(new String[0]));
            }
        }
    }

    public static void main(String args[])
    {
        try {
            Config cf = new ConfigFile(new File(args[0]));

            System.out.println("Keys: ");
            for (String key : cf.getKeys()){
                String vs[] = cf.keys.get(key);

                for (int vidx = 0; vidx < vs.length; vidx++)
                    System.out.printf("  %-40s : %s\n", vidx == 0 ? key : "", vs[vidx]);
            }

            System.out.println("Abstract keys: ");
            for (String key : cf.getAbstractKeys()) {

                String vs[] = cf.abstractKeys.get(key);

                for (int vidx = 0; vidx < vs.length; vidx++)
                    System.out.printf("  %-40s : %s\n", vidx == 0 ? key : "", vs[vidx]);
            }
        } catch (IOException ex) {
            System.out.println("ex: "+ex);
        }
    }

}
