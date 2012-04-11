package april.config;

import java.util.*;
import java.io.*;

import april.util.*;

/**
 * A set of key/value configuration data.
 **/
public class Config
{
    HashMap<String, String[]> keys = new VerboseHashMap<String, String[]>();
    HashMap<String, String[]> abstractKeys = new VerboseHashMap<String, String[]>();

    String       prefix; // either empty or has a trailing "." so that
                         // prefix+key is always well-formed

    String       basePath;   // root directory for any paths that aren't fully specified

    Config       root;   // a config whose prefix is empty ("")

    public boolean verbose = EnvUtil.getProperty("april.config.debug", false);

    public Config()
    {
        this.prefix = "";
        this.basePath = "";
        this.root = this;
    }

    class VerboseHashMap<K,V> extends HashMap<K,V>
    {
        public V get(Object k)
        {
            V v = super.get(k);
            if (verbose) {
                StringBuffer sb = new StringBuffer();
                if (v == null)
                    sb.append("null");
                else {
                    if (v != null) {
                        String vs[] = (String[]) v;
                        for (String s : vs) {
                            sb.append(s+" ");
                        }
                    }
                }

                System.out.printf("Config: %s = %s\n", k, sb.toString());
            }
            return v;
        }
    }

    public Config getChild(String childprefix)
    {
        Config child = new Config();
        child.keys = keys;

        child.prefix = this.prefix;
        if (child.prefix.length() > 0 && !child.prefix.endsWith("."))
            child.prefix = child.prefix+".";
        child.prefix = child.prefix+childprefix+".";

        child.basePath = basePath;
        child.root = root;

        return child;
    }

    public Config getRoot()
    {
        return root;
    }

    public String[] getKeys()
    {
        ArrayList<String> subkeys = new ArrayList<String>();

        for (String key : keys.keySet()) {
            if (key.length() <= prefix.length())
                continue;
            if (key.startsWith(prefix))
                subkeys.add(key.substring(prefix.length()));
        }

        return subkeys.toArray(new String[subkeys.size()]);
    }

    // Returns keys beginning with ':', which are normally hidden
    public String[] getAbstractKeys()
    {
        ArrayList<String> subkeys = new ArrayList<String>();

        for (String key : abstractKeys.keySet()) {
            if (key.length() <= prefix.length())
                continue;
            if (key.startsWith(prefix))
                subkeys.add(key.substring(prefix.length()));
        }

        return subkeys.toArray(new String[subkeys.size()]);
    }

    public boolean hasKey(String key)
    {
        return keys.containsKey(prefix + key);
    }

    void missingRequired(String key)
    {
        System.out.println("Config: Required key '"+key+"' missing.");
        assert(false);
    }

    ////////////////////////////
    // int
    public int[] getInts(String key, int defaults[])
    {
        String vs[] = keys.get(prefix + key);
        if (vs == null)
            return defaults;

        int v[] = new int[vs.length];
        for (int i = 0; i < vs.length; i++) {
            v[i] = Integer.parseInt(vs[i]);
        }

        return v;
    }

    public int[] getInts(String key)
    {
        return getInts(key, null);
    }

    public int[] requireInts(String key)
    {
        int v[] = getInts(key, null);
        if (v == null)
            missingRequired(key);
        return v;
    }

    public int getInt(String key, int def)
    {
        return getInts(key, new int[] { def})[0];
    }

    public int requireInt(String key)
    {
        int v[] = getInts(key, null);

        if (v == null)
            missingRequired(key);

        return v[0];
    }

    public void setInt(String key, int v)
    {
        setInts(key, new int[] { v });
//        source.setInts(key, new int[] {v});
    }

    public void setInts(String key, int v[])
    {
        String[] values = new String[v.length];
        for (int i = 0; i < v.length; ++i)
        {
            values[i] = "" + v[i];
        }
        keys.put(key, values);
//        source.setInts(key, v);
    }

    ///////////////////////////
    // Paths
    public String getPath(String key, String def)
    {
        String path = getString(key, def);
        if (path == null)
            return def;

        path = path.trim(); // remove white space
        if (!path.startsWith("/"))
            return basePath + path;

        return path;
    }

    public String getPath(String key)
    {
        return getPath(key, null);
    }

    ///////////////////////////
    // String
    public String[] getStrings(String key, String defaults[])
    {
        String v[] = keys.get(prefix + key);
        return (v==null) ? defaults : v;
    }

    public String[] getStrings(String key)
    {
        return getStrings(key, null);
    }

    public String[] requireStrings(String key)
    {
        String v[] = getStrings(key, null);
        if (v == null)
            missingRequired(key);
        return v;
    }

    public String getString(String key)
    {
        return getString(key, null);
    }

    public String getString(String key, String def)
    {
        return getStrings(key, new String[] { def })[0];
    }

    public String requireString(String key)
    {
        String v[] = getStrings(key, null);
        if (v == null)
            missingRequired(key);

        return v[0];
    }

    public void setString(String key, String v)
    {
        keys.put(key, new String[] { v });
    }

    public void setStrings(String key, String v[])
    {
        keys.put(key, v);
    }

    //For now, we can access Abstract keys individually:
    public String[] getAbstractStrings(String abstractKey)
    {
        return abstractKeys.get(prefix + abstractKey);
    }

    public String getAbstractString(String abstractKey)
    {
        return getAbstractStrings(abstractKey)[0];
    }

    ////////////////////////////
    // boolean
    public boolean[] getBooleans(String key, boolean defaults[])
    {
        String vs[] = keys.get(prefix + key);
        if (vs == null)
            return defaults;

        boolean v[] = new boolean[vs.length];
        for (int i = 0; i < vs.length; i++) {
            v[i] = Boolean.parseBoolean(vs[i]);
        }

        return v;
    }

    public boolean[] getBooleans(String key)
    {
        return getBooleans(key, null);
    }

    public boolean[] requireBooleans(String key)
    {
        boolean v[] = getBooleans(key, null);
        if (v == null)
            missingRequired(key);
        return v;
    }

    public boolean getBoolean(String key, boolean def)
    {
        return getBooleans(key, new boolean[] { def })[0];
    }

    public boolean requireBoolean(String key)
    {
        boolean v[] = getBooleans(key);
        if (v == null)
            missingRequired(key);
        return v[0];
    }

    public void setBoolean(String key, boolean v)
    {
        keys.put(prefix+key, new String[] {""+v});
    }

    public void setBooleans(String key, boolean v[])
    {
        String[] values = new String[v.length];
        for (int i = 0; i < v.length; ++i)
        {
            values[i] = "" + v[i];
        }
        keys.put(key, values);
    }

    ////////////////////////////
    // double
    public double[] getDoubles(String key, double defaults[])
    {
        String vs[] = keys.get(prefix + key);
        if (vs == null)
            return defaults;

        double v[] = new double[vs.length];
        for (int i = 0; i < vs.length; i++) {
            v[i] = Double.parseDouble(vs[i]);
        }

        return v;
    }

    public double[] getDoubles(String key)
    {
        return getDoubles(key, null);
    }

    public double[] requireDoubles(String key)
    {
        double v[] = getDoubles(key, null);
        if (v == null)
            missingRequired(key);
        return v;
    }

    public double getDouble(String key, double def)
    {
        return getDoubles(key, new double[] { def })[0];
    }

    public double requireDouble(String key)
    {
        double v[] = getDoubles(key, null);
        if (v == null)
            missingRequired(key);
        return v[0];
    }

    public void setDouble(String key, double v)
    {
        keys.put(key, new String[] { "" + v });
    }

    public void setDoubles(String key, double v[])
    {
        String[] values = new String[v.length];
        for (int i = 0; i < v.length; ++i)
        {
            values[i] = "" + v[i];
        }
        keys.put(key, values);
    }

    ////////////////////////////
    // double
    public byte[] getBytes(String key, byte defaults[])
    {
        String lines[] = getStrings(key);
        if (lines == null)
            return defaults;

        return Base64.decode(lines);
    }

    public byte[] requireBytes(String key)
    {
        byte v[] = getBytes(key, null);
        if (v == null)
            missingRequired(prefix+key);
        return v;
    }

    public void setBytes(String key, byte v[])
    {
        String[] values = new String[v.length];
        for (int i = 0; i < v.length; ++i)
        {
            values[i] = "" + v[i];
        }
        keys.put(key, values);
    }

    public void merge(Config includedConfig)
    {
        for (String key : includedConfig.keys.keySet()) {
            keys.put(key, includedConfig.keys.get(key));
        }
    }

    ////////////////////////////////////////////////////
}
