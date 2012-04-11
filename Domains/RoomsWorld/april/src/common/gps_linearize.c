/* gps_linearize.c:
 *
 * Functions for projecting GPS coordinates into a planar coordinate system
 * centered at a particular GPS point.  This is suitable for geometry
 * computations using GPS in local neighborhoods of a few kilometers.
 */

#include <stdint.h>
#include <stdlib.h>
#include <math.h>

#include <common/math_util.h>
#include "gps_linearize.h"

#define REQ 6378135.0
#define RPO 6356750.0

/* Useful links:
   http://www.movable-type.co.uk/scripts/LatLongVincenty.html
   http://en.wikipedia.org/wiki/Earth_radius
*/

void gps_linearize_init(gps_linearize_t *gl, const double ll_deg[2])
{
    gl->lat0_deg = ll_deg[0];
    gl->lon0_deg = ll_deg[1];

    double a = 6378135;  // R_equator
    double b = 6356750;  // R_polar

    double lat_rad = to_radians(ll_deg[0]);

    // this is the best radius approximation, agnostic of direction
    // we don't use this anymore.
    //    gl->radius = a*a*b / (sq(a*cos(lat_rad)) + sq(b*sin(lat_rad)));

    // best radius approximation in ns and ew direction.
    gl->radius_ns = sq(a*b) / pow((sq(a*cos(lat_rad))) + sq(b*sin(lat_rad)), 1.5);
    gl->radius_ew = a*a / sqrt(sq(a*cos(lat_rad)) + sq(b*sin(lat_rad)));
}

int gps_linearize_to_xy(gps_linearize_t *gl, const double ll_deg[2], double xy[2])
{
    double dlat = to_radians(ll_deg[0] - gl->lat0_deg);
    double dlon = to_radians(ll_deg[1] - gl->lon0_deg);

    xy[0] = sin(dlon) * gl->radius_ew * cos(to_radians(gl->lat0_deg));
    xy[1] = sin(dlat) * gl->radius_ns;
    
    return 0;
}

int gps_linearize_to_lat_lon(gps_linearize_t *gl, const double xy[2], double ll_deg[2])
{
    double dlat = asin(xy[1] / gl->radius_ns);
    ll_deg[0] = to_degrees(dlat) + gl->lat0_deg;

    double dlon = asin(xy[0] / gl->radius_ew / cos(to_radians(gl->lat0_deg)));
    ll_deg[1] = to_degrees(dlon) + gl->lon0_deg;

    return 0;
}
