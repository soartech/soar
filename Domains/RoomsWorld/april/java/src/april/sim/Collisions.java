package april.sim;

import april.jmat.*;

// limitations: The transforms must be rigid-body + a uniform scaling on all three axes.
public class Collisions
{
    public static boolean collision(Shape _sa, Shape _sb)
    {
        double T[][] = LinAlg.identity(4);
        return collision(_sa, T, _sb, T);
    }

    public static boolean collision(Shape _sa, double Ta[][], Shape _sb, double Tb[][])
    {
        if (_sa instanceof CompoundShape) {
            return collision((CompoundShape) _sa, Ta, _sb, Tb);
        }
        if (_sb instanceof CompoundShape) {
            return collision((CompoundShape) _sb, Tb, _sa, Ta);
        }
        if (_sa instanceof BoxShape && _sb instanceof BoxShape) {
            return collision((BoxShape) _sa, Ta, (BoxShape) _sb, Tb);
        }
        if (_sa instanceof SphereShape && _sb instanceof SphereShape) {
            return collision((SphereShape) _sa, Ta, (SphereShape) _sb, Tb);
        }
        if (_sa instanceof BoxShape && _sb instanceof SphereShape) {
            return collision((BoxShape) _sa, Ta, (SphereShape) _sb, Tb);
        }
        if (_sb instanceof BoxShape && _sa instanceof SphereShape) {
            return collision((BoxShape) _sb, Tb, (SphereShape) _sa, Ta);
        }
        assert(false);
        return false;
    }

    static final void backup(double A[][], double b[])
    {
        for (int i = 0; i < 4; i++)
            for (int j = 0; j < 4; j++)
                b[i*4+j] = A[i][j];
    }

    static final void restore(double A[][], double b[])
    {
        for (int i = 0; i < 4; i++)
            for (int j = 0; j < 4; j++)
                A[i][j] = b[i*4+j];
    }

    public static boolean collision(CompoundShape sa, double Ta[][], Shape _sb, double Tb[][])
    {
        double tmp[] = new double[16];

        backup(Ta, tmp);
        boolean ret = false;

        for (Object op : sa.ops) {
            if (op instanceof double[][]) {
                LinAlg.timesEquals(Ta, (double[][]) op);
            } else if (op instanceof Shape) {
                if (Collisions.collision((Shape) op, Ta, _sb, Tb)) {
                    ret = true;
                    break;
                }
            } else {
                System.out.println(op);
                assert(false);
            }
        }

        restore(Ta, tmp);
        return ret;
    }

    public static boolean collision(BoxShape sa, double Ta[][], SphereShape sb, double Tb[][])
    {
        // TODO: fast check first?

        // p_g = Ta p_a
        // p_g = Tb p_b
        // Ta p_a = Tb p_b
        // p_a = inv(Ta) Tb p_b

        // project the sphere's center into the box's coordinate system.
        double T[][] = LinAlg.matrixAB(LinAlg.inverse(Ta), Tb);

        double ex = Math.max(0, Math.abs(T[0][3]) - sa.sxyz[0]/2);
        double ey = Math.max(0, Math.abs(T[1][3]) - sa.sxyz[1]/2);
        double ez = Math.max(0, Math.abs(T[2][3]) - sa.sxyz[2]/2);

        double e2 = ex*ex + ey*ey + ez*ez;

        // project sphere's radius into this box's coordinate
        // system. (Necessary because there may be a scale
        // transformation!)

        double scale2 = LinAlg.sq(T[0][0]) + LinAlg.sq(T[1][0]) + LinAlg.sq(T[2][0]);
        double r2 = scale2 * LinAlg.sq(sb.r);

        return e2 < r2;
    }

    public static boolean collision(SphereShape sa, double Ta[][], SphereShape sb, double Tb[][])
    {
        double d = Math.sqrt(LinAlg.sq(Ta[0][3]-Tb[0][3]) +
                             LinAlg.sq(Ta[1][3]-Tb[1][3]) +
                             LinAlg.sq(Ta[2][3]-Tb[2][3]));

        double scalea2 = LinAlg.sq(Ta[0][0]) + LinAlg.sq(Ta[1][0]) + LinAlg.sq(Ta[2][0]);
        double scaleb2 = LinAlg.sq(Tb[0][0]) + LinAlg.sq(Tb[1][0]) + LinAlg.sq(Tb[2][0]);

        return (d - Math.sqrt(scalea2)*sa.r - Math.sqrt(scaleb2)*sb.r) <= 0;
    }

    public static boolean collision(BoxShape sa, double Ta[][], BoxShape sb, double Tb[][])
    {
        // TODO: fast check first?

        BoxShape sa2 = sa.transform(Ta);
        BoxShape sb2 = sb.transform(Tb);

        if (LinAlg.faceBelowPoints(sa2.planes, sb2.vertices) ||
            LinAlg.faceBelowPoints(sb2.planes, sa2.vertices))
            return false;

        return true;
    }

    public static double collisionDistance(double pos[], double dir[], Shape _s, double T[][])
    {
        if (_s instanceof SphereShape)
            return collisionDistance(pos, dir, (SphereShape) _s, T);
        if (_s instanceof BoxShape)
            return collisionDistance(pos, dir, (BoxShape) _s, T);
        if (_s instanceof CompoundShape)
            return collisionDistance(pos, dir, (CompoundShape) _s, T);

        assert(false);
        return Double.MAX_VALUE;
    }

    public static double collisionDistance(double pos[], double dir[], SphereShape s, double T[][])
    {
        // unit vector from pos to center of sphere
        double xyz[] = new double[] { T[0][3], T[1][3], T[2][3] };
        double e[] = LinAlg.normalize(LinAlg.subtract(xyz, pos));

        // cosine of theta between dir and vector that would lead to
        // center of circle
        double costheta = LinAlg.dotProduct(e, dir);

        // law of cosines gives us:
        // r^2 = x^2 + d^2 - 2xd cos(theta)
        double d = LinAlg.distance(xyz, pos);

        // solve for x using quadratic formula
        double A = 1;
        double B = -2*d*costheta;
        double C = d*d - s.r*s.r;

        // no collision
        if (B*B - 4*A*C < 0)
            return Double.MAX_VALUE;

        double x1 = (-B - Math.sqrt(B*B - 4 * A * C)) / (2*A);
        if (x1 >= 0)
            return x1;

        double x2 = (-B + Math.sqrt(B*B - 4 * A * C)) / (2*A);
        if (x2 >= 0)
            return x2;

        return Double.MAX_VALUE;
    }

    public static double collisionDistance(double pos[], double dir[], CompoundShape s, double T[][])
    {
        double d = Double.MAX_VALUE;
        double tmp[] = new double[16];

        backup(T, tmp);

        for (Object op : s.ops) {
            if (op instanceof double[][]) {
                LinAlg.timesEquals(T, (double[][]) op);
            } else if (op instanceof Shape) {
                d = Math.min(d, collisionDistance(pos, dir, (Shape) op, T));
            } else {
                System.out.println(op);
                assert(false);
            }
        }

        restore(T, tmp);
        return d;
    }

    public static double collisionDistance(double _pos[], double _dir[], BoxShape s, double T[][])
    {
        if (true) {
            double pos[] = LinAlg.transformInverse(T, _pos);
            double dir[] = LinAlg.transformInverseRotateOnly(T, _dir);

            return LinAlg.rayCollisionBox(pos, dir, s.sxyz);
        } else {
            double Tinv[][] = LinAlg.inverse(T);

            double pos[] = LinAlg.transform(Tinv, _pos);
            double dir[] = LinAlg.transformRotateOnly(Tinv, _dir);

            LinAlg.print(dir);

            return LinAlg.rayCollisionBox(pos, dir, s.sxyz);
        }
    }

    public static void main(String args[])
    {
        double p[] = new double[] { 1, 0, 0, -1 };
        LinAlg.print(LinAlg.transformPlane(LinAlg.matrixAB(LinAlg.translate(1, 0, 0),
                                                           LinAlg.rotateZ(Math.PI/4)), p));

        CompoundShape a = new CompoundShape(LinAlg.translate(5, 0, 0),
                                            new BoxShape(new double[] { 2, 2, 2}));

        CompoundShape b = new CompoundShape(LinAlg.translate(7.2, 0, 0),
                                            LinAlg.rotateZ(Math.PI/4),
                                            new BoxShape(new double[] { 10, .1, .1}));

        System.out.println(Collisions.collision(a, LinAlg.identity(4), b, LinAlg.identity(4)));
    }
}
