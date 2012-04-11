package april.vis;

/** Helper class used to generate help dialog. **/
public class HelpOutput
{
    // action
    // e.g.: SHIFT | LEFT | CLICK
    public static final int LEFT = 1, MIDDLE = 2, RIGHT = 3, WHEEL = 7, CLICK = 256, DRAG = 512;
    public static final int SHIFT = 8192, CTRL = 16384, CONTROL = 32768, ALT = 65536;

    StringBuffer sb = new StringBuffer();

    static final String format(int mods, String key)
    {
        StringBuffer sb = new StringBuffer();

        if ((mods & SHIFT)>0)
            sb.append("shift + ");
        if ((mods & CTRL)>0)
            sb.append("ctrl + ");
        if ((mods & ALT)>0)
            sb.append("alt + ");

        switch (mods&127) {
            case LEFT:
                sb.append("left ");
                break;
            case MIDDLE:
                sb.append("middle ");
                break;
            case RIGHT:
                sb.append("right ");
                break;
            case WHEEL:
                sb.append("wheel ");
                break;
        }

        if ((mods & CLICK) > 0)
            sb.append("click");

        if ((mods & DRAG) > 0)
            sb.append("drag");

        if (key.length() == 1)
            sb.append(String.format("'"+key+"'"));
        if (key.length() > 1)
            sb.append(key);

        return sb.toString().trim();
    }

    public void beginKeyboardCommands(VisCanvasEventHandler eh)
    {
        sb.append(String.format("\n%s: Keyboard Commands\n", eh.getName()));
    }

    public void beginMouseCommands(VisCanvasEventHandler eh)
    {
        sb.append(String.format("\n%s: Mouse Commands\n", eh.getName()));
    }

    public void addKeyboardCommand(VisCanvasEventHandler eh, String key, int mods, String description)
    {
        sb.append(String.format("%20s  %s\n", format(mods,key), description));
    }

    public void addMouseCommand(VisCanvasEventHandler eh, int mods, String description)
    {
        sb.append(String.format("%20s  %s\n", format(mods,""), description));
    }

    public String toString()
    {
        return sb.toString();
    }
}
