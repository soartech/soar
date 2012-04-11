package april.util;

import java.awt.*;
import java.util.*;

/** LayoutManager similar to FlowLayout, except oriented vertically. **/
public class VFlowLayout implements LayoutManager
{
    int maxx, maxy;

    public void	addLayoutComponent(String name, Component comp)
    {
    }

    public void	layoutContainer(Container parent)
    {
        maxx = 0;
        maxy = 0;

        Component comps[] = parent.getComponents();

        for (Component comp : comps) {
            Dimension psize = comp.getPreferredSize();
            maxx = (int) Math.max(maxx, psize.getWidth());
        }

        for (Component comp : comps) {
            Dimension psize = comp.getPreferredSize();

            int thisheight = (int) psize.getHeight();
            comp.setBounds(0, maxy, maxx, thisheight);
            maxy += thisheight;
        }
    }

    public Dimension minimumLayoutSize(Container parent)
    {
        return new Dimension(1,1);
    }

    public Dimension preferredLayoutSize(Container parent)
    {
        layoutContainer(parent);

        return new Dimension(maxx, maxy);
    }

    public void removeLayoutComponent(Component comp)
    {
    }
}
