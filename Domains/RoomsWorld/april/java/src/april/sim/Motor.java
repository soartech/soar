package april.sim;

/** Simulates a DC direct-drive motor. **/
public class Motor
{
    boolean input_connected;
    double input_volts;

    // Motor state
    double rad_per_sec;
    double amps;

    // Motor characteristics
    public double torque_constant = 3.0;     // torque (Nm) per amp
    public double emf_constant = 2.0;        // volts per rad_per_sec
    public double winding_resistance = 5.5;  // ohms
    public double inertia = 0.5;             // kg*m^2
    public double drag_constant = 1.0;       // drag (Nm per rad_per_sec) ( >= 0)

    public void setVoltage(double v)
    {
        setVoltage(v, true);
    }

    public void setVoltage(double v, boolean connected)
    {
        this.input_volts = v;
        this.input_connected = connected;
    }

    public double getRadPerSec()
    {
        return rad_per_sec;
    }

    public double getAmps()
    {
        return amps;
    }

    public void update(double dt)
    {
        double volts_emf = rad_per_sec * emf_constant;
        this.amps = (input_volts - volts_emf) / winding_resistance;

        if (!input_connected)
            this.amps = 0;

        double torque0 = this.amps * torque_constant;
        double torque_drag = rad_per_sec * drag_constant;

        double torque_net = torque0 - torque_drag;
        double acceleration = torque_net / inertia;

        rad_per_sec += acceleration * dt;
    }

    public static void main(String args[])
    {
        double t = 0;
        double dt = 0.1;

        Motor m = new Motor();
        m.setVoltage(12);

        while (t < 20.0) {
            if (t >= 10.0)
                m.setVoltage(0, false);

            m.update(dt);
            t += dt;
            System.out.printf("%10.3f : %10.3f %10.3f\n", t, m.getRadPerSec(), m.getAmps());
        }
    }
}
