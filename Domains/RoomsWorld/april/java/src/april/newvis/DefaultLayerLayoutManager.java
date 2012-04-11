package april.newvis;

import java.util.*;
import april.jmat.*;

public class DefaultLayerLayoutManager implements VisLayerLayoutManager
{
    VisCanvas vc;
    ArrayList<FPosition> fposes = new ArrayList<FPosition>();

    static class FPosition
    {
        VisLayer layer;
        double fviewport[];
    }

    public DefaultLayerLayoutManager(VisCanvas vc)
    {
        this.vc = vc;
    }

    public void addLayer(VisLayer layer, double fviewport[])
    {
        FPosition fpos = new FPosition();
        fpos.layer = layer;
        fpos.fviewport = LinAlg.copy(fviewport);
        fposes.add(fpos);

        vc.draw();
    }

    public ArrayList<VisLayerLayoutManager.Position> layout(int viewport[])
    {
        ArrayList<VisLayerLayoutManager.Position> poses = new ArrayList<VisLayerLayoutManager.Position>();

        for (FPosition fpos : fposes) {
            VisLayerLayoutManager.Position pos = new VisLayerLayoutManager.Position();

            pos.layer = fpos.layer;
            pos.viewport = new int[] { (int) (viewport[2]*fpos.fviewport[0]),
                                       (int) (viewport[3]*fpos.fviewport[1]),
                                       (int) (viewport[2]*fpos.fviewport[2]),
                                       (int) (viewport[3]*fpos.fviewport[3]) };
            poses.add(pos);
        }

        return poses;
    }
}
