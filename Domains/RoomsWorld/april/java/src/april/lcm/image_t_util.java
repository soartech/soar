package april.lcm;

import java.awt.Color;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Iterator;

import javax.imageio.IIOImage;
import javax.imageio.ImageIO;
import javax.imageio.ImageReader;
import javax.imageio.ImageTypeSpecifier;
import javax.imageio.ImageWriteParam;
import javax.imageio.ImageWriter;
import javax.imageio.metadata.IIOInvalidTreeException;
import javax.imageio.metadata.IIOMetadata;
import javax.imageio.plugins.jpeg.JPEGImageReadParam;
import javax.imageio.plugins.jpeg.JPEGImageWriteParam;
import javax.imageio.stream.MemoryCacheImageInputStream;
import javax.imageio.stream.MemoryCacheImageOutputStream;

import org.w3c.dom.Node;

import april.lcmtypes.image_t;

public class image_t_util
{
    public static final int FORMAT_JPEG = 1196444237;
    public static final int FORMAT_RGB = 859981650;
    public static final int FORMAT_MJP0 = 0x30504A4D;
    public static final int FORMAT_MJP1 = 0x31504A4D;
    public static final int FORMAT_MJP2 = 0x32504A4D;
    public static final int FORMAT_MJP3 = 0x33504A4D;
    public static final int FORMAT_MJP4 = 0x34504A4D;
    public static final int FORMAT_MJP5 = 0x35504A4D;
    public static final int FORMAT_MJP6 = 0x36504A4D;
    public static final int FORMAT_MJP7 = 0x37504A4D;
    public static final int FORMAT_MJP8 = 0x38504A4D;
    public static final int FORMAT_MJP9 = 0x39504A4D;

    static final JPEGTableFactory tableFactory = new JPEGTableFactory();
    static final IIOMetadata customMetaData;

    static {
        //
        // Construct custom meta-data used for MJPEG encoding
        //
        final Iterator<ImageWriter> iter = ImageIO.getImageWritersByMIMEType("image/jpeg");
        final ImageWriter writer = iter.next();

        customMetaData = writer.getDefaultImageMetadata(
                ImageTypeSpecifier.createFromBufferedImageType(BufferedImage.TYPE_INT_RGB), null);
        try {
            removeTables(customMetaData);

        } catch (final IIOInvalidTreeException ex) {
            System.out.println("ERR: Could not create custom metadata for MJPEG: " + ex);
            System.exit(1);
        }
    }

    public static BufferedImage decode(image_t v) throws IOException
    {
        switch (v.pixelformat) {

            case FORMAT_JPEG: // JPEG
                return ImageIO.read(new ByteArrayInputStream(v.image));

            case FORMAT_RGB:  // raw RGB
                return decodeRGB(v);

            case FORMAT_MJP0: // Motion JPEG (JPEG without headers)
            case FORMAT_MJP1:
            case FORMAT_MJP2:
            case FORMAT_MJP3:
            case FORMAT_MJP4:
            case FORMAT_MJP5:
            case FORMAT_MJP6:
            case FORMAT_MJP7:
            case FORMAT_MJP8:
            case FORMAT_MJP9:
            	return decodeMJPEG(v);
            
            default:        // uncompressed gray scale.
                return decodeRAW(v);
        }
    }

    /** Quality: 0 = low, 1 = high **/
    public static image_t encodeJPEG(BufferedImage bi, float quality) throws IOException
    {
        image_t v = new image_t();
        v.width = (short) bi.getWidth();
        v.height = (short) bi.getHeight();
        v.stride = v.width; // unused
        v.pixelformat = FORMAT_JPEG;

        Iterator<ImageWriter> iter = ImageIO.getImageWritersByMIMEType("image/jpeg");
        ImageWriter writer = iter.next();

        ImageWriteParam params = writer.getDefaultWriteParam();
        params.setCompressionMode(ImageWriteParam.MODE_EXPLICIT);
        params.setCompressionQuality(quality);

        ByteArrayOutputStream bouts = new ByteArrayOutputStream();

        try {
            writer.setOutput(new MemoryCacheImageOutputStream(bouts));
            writer.write(null, new IIOImage(bi, null, null), params);
        } catch (IOException ex) {
            System.out.println("WRN: "+ex);
            return null;
        }

        v.image = bouts.toByteArray();
        v.size = v.image.length;

        return v;
    }

    /** Quality: 0 = low, 1 = high **/
    public static image_t encodeMJPEG(BufferedImage bi, float quality) throws IOException
    {
        // Quantize quality setting into 10 levels [0.0, 0.9]
        final int qualityQuantum = Math.min((int)(quality*10), 9);
        quality = (qualityQuantum + 1) / 10f;
        
        final image_t v = new image_t();
        v.width = (short) bi.getWidth();
        v.height = (short) bi.getHeight();
        v.stride = v.width; // unused
        v.pixelformat = FORMAT_MJP0 | ((0x30 + qualityQuantum) << 24) ;

        tableFactory.setQuality( quality );

        final Iterator<ImageWriter> iter = ImageIO.getImageWritersByMIMEType("image/jpeg");
        final ImageWriter writer = iter.next();

        final JPEGImageWriteParam param = (JPEGImageWriteParam) writer.getDefaultWriteParam();
        param.setCompressionMode(ImageWriteParam.MODE_COPY_FROM_METADATA);
        param.setEncodeTables(tableFactory.getQTables(), tableFactory.getDCHuffTables(),
                tableFactory.getACHuffTables());

        final ByteArrayOutputStream bouts = new ByteArrayOutputStream();

        try {
            writer.setOutput(new MemoryCacheImageOutputStream(bouts));
            writer.write(null, new IIOImage(bi, null, customMetaData), param);

        }
        catch (final IOException ex) {
            System.out.println("WRN: " + ex);
        }
        finally {
            writer.dispose();
        }

        v.image = bouts.toByteArray();
        v.size = v.image.length;

        return v;
    }

    static final int grayToRGB(byte v)
    {
        int g = v&0xff;
        return (g<<16)|(g<<8)|g;
    }

    static BufferedImage decodeRAW(image_t v)
    {
        BufferedImage bi = new BufferedImage(v.width, v.height, BufferedImage.TYPE_INT_RGB);

        for (int y = 0; y < v.height; y++) {
            for (int x = 0; x < v.width; x++) {
                bi.setRGB(x, y, grayToRGB(v.image[x+y*v.stride]));
            }
        }

        return bi;
    }

    static BufferedImage decodeRGB(image_t v)
    {
        BufferedImage bi = new BufferedImage(v.width, v.height, BufferedImage.TYPE_INT_RGB);

        for (int y = 0; y < v.height; y++) {
            for (int x = 0; x < v.width; x++) {
                int index = 3*x+y*v.stride;
                byte r = v.image[index +0];
                byte g = v.image[index +1];
                byte b = v.image[index +2];


                int r_int = r & 0xff;
                int g_int = g & 0xff;
                int b_int = b & 0xff;

                Color col = new Color(r_int,g_int,b_int);
                bi.setRGB(x, y, col.getRGB());
            }
        }

        return bi;
    }

    static BufferedImage decodeMJPEG(image_t v)
    {
        try {
            final ImageReader reader = ImageIO.getImageReadersBySuffix("jpg").next();
            reader.setInput(new MemoryCacheImageInputStream(new ByteArrayInputStream(v.image)));

            final float quality = ((v.pixelformat >> 24) - 0x30) / 10f;
            tableFactory.setQuality(quality);

            final JPEGImageReadParam param = new JPEGImageReadParam();
            param.setDecodeTables(tableFactory.getQTables(), tableFactory.getDCHuffTables(),
                    tableFactory.getACHuffTables());

            final BufferedImage im = reader.read(0, param);
            return im;
        } catch (final IOException ex) {
            System.out.println("ImageConvert: MJPG decode failed: " + ex);
            return null;
        }
    }

    private static void removeTables(IIOMetadata metadata) throws IIOInvalidTreeException
    {
        final String format = metadata.getNativeMetadataFormatName();
        if (!"javax_imageio_jpeg_image_1.0".equals(format))
            throw new IllegalArgumentException("Unrecognized meta format: " + format);

        // Chop off everything under the marker sequence tag
        final Node root = metadata.getAsTree(format);
        final Node markerSeq = root.getFirstChild().getNextSibling();

        Node child = markerSeq.getFirstChild();
        while (child != null) {
            final Node nextChild = child.getNextSibling();
            markerSeq.removeChild(child);
            child = nextChild;
        }

        // Commit changes
        metadata.setFromTree(format, root);
    }
}
