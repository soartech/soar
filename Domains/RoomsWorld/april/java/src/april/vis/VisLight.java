package april.vis;

import april.jmat.*;

public class VisLight
{
    float position[];
    float ambient[], diffuse[], specular[];

    public VisLight(float position[], float ambient[], float diffuse[], float specular[])
    {
        this.position = LinAlg.copy(position);
        this.ambient = LinAlg.copy(ambient);
        this.diffuse = LinAlg.copy(diffuse);
        this.specular = LinAlg.copy(specular);
    }
}
