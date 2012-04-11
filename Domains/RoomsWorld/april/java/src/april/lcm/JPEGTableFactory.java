package april.lcm;

import javax.imageio.plugins.jpeg.JPEGHuffmanTable;
import javax.imageio.plugins.jpeg.JPEGQTable;

/**
 * Creates JPEG quantization tables for different quality settings
 *
 * @author rpradeep
 */
public class JPEGTableFactory
{
    float quality = 0.5f;
    JPEGQTable[] qTables = { JPEGQTable.K2Chrominance };
    final JPEGHuffmanTable[] dcHuffmanTables = { JPEGHuffmanTable.StdDCChrominance, JPEGHuffmanTable.StdDCLuminance };
    final JPEGHuffmanTable[] acHuffmanTables = { JPEGHuffmanTable.StdACChrominance, JPEGHuffmanTable.StdACLuminance };

    public JPEGQTable[] getQTables()
    {
        return qTables;
    }

    public JPEGHuffmanTable[] getDCHuffTables()
    {
        return dcHuffmanTables;
    }

    public JPEGHuffmanTable[] getACHuffTables()
    {
        return acHuffmanTables;
    }

    /**
     * Adjust the quantization tables based on required output quality
     *
     * @param q
     *            0 produces lowest quality output, 0.5 good quality and 1.0 the best possible quality. A setting of 1.0
     *            corresponds to the default quality settings in java (which is observably very good).
     * @return Returns true if the tables were updated. false otherwise (when the current quality is the same as the
     *         input parameter q).
     */
    public boolean setQuality(float q)
    {
        if (q == quality)
            return false;

        q = Math.max(q, 0.01f);
        q = Math.min(q, 1);
        q = (q < 0.5f) ? (0.5f / q) : (1.5f - q);

        for (int i = 0; i < qTables.length; ++i)
            qTables[i] = JPEGQTable.K2Chrominance.getScaledInstance(q, false);

        return true;
    }
}
