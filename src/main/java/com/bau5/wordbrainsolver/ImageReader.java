package com.bau5.wordbrainsolver;

import com.typesafe.config.Config;
import com.typesafe.config.ConfigFactory;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.*;
import java.util.ArrayList;
import javax.imageio.ImageIO;

import org.bytedeco.javacpp.BytePointer;
import static org.bytedeco.javacpp.lept.*;
import org.bytedeco.javacpp.tesseract.*;


/**
 * Created by Rick on 12/6/15.
 */
public class ImageReader {
    final TessBaseAPI api;

    public static void main(String[] args) {
        BoardProperties props = new ImageReader().processScreenshot("4x4.png");
        System.out.println(props.getOutput());
    }

    public ImageReader() {
        this.api = new TessBaseAPI();
        if (this.api.Init(".", "ENG") != 0) {
            System.err.println("Failed initializing Tesseract");
            System.exit(1);
        }

    }

    public BoardProperties processScreenshot(String fileName) {
        Config conf = ConfigFactory.load();
        String ret = null;
        BoardProperties boardProperties = null;

        try {
            boardProperties = getBoardProperties(conf, fileName);
            boardProperties.saveImages(conf);
            boardProperties.saveProperties(conf);

            String readerOutput = readImage(boardProperties.board);
            String processed = readerOutput.toLowerCase().replace(" ", "").replace("\n", "");

            if (processed.length() == boardProperties.getTotalTiles()) {
                ret = readerOutput.toLowerCase();
            } else {
                throw new IllegalStateException(String.format("Length does not match! Found %d, expected %d. Output was \n%s",
                                    processed.length(), boardProperties.getTotalTiles(), readerOutput));
            }
        } catch (IOException ioex) {
            System.err.println("Issue encountered while working with files.");
            ioex.printStackTrace();
        } catch (IllegalStateException isex) {
            System.err.println("Failed reading board!");
            isex.printStackTrace();
        }

        if (ret != null) {
            // Some common Tesseract derps in our case
            String save = ret.replace("|", "i");

            boardProperties.setOutput(save);
            return boardProperties;
        } else {
            return null;
        }
    }

    /**
     *
     * @param conf
     * @return an image that is all the tiles stitched together, without the space in between.
     */
    public BoardProperties getBoardProperties(Config conf, String fileName) throws IOException {
        BufferedImage image = getCroppedBoard(conf, fileName);
        String tileColor = conf.getString(prefixed("colors.tile"));

        int tileYStart = -1;
        int tileXStart = -1;

        // Find the y location in the cropped image of the tile
        outer : for (int i = 0; i < image.getWidth(); i++) {
            for (int j = 0; j < image.getHeight(); j++) {
                if (Integer.toHexString(image.getRGB(i, j)).equals(tileColor)) {
                    tileXStart = i;
                    tileYStart = j;
                    break outer;
                }
            }
        }

        boolean tileFlag = false;
        int tilesInRow = 0;
        int tileXEnd = -1;

        for (int width = 0; width < image.getWidth(); width++) {
            if (!tileFlag) {
                if (Integer.toHexString(image.getRGB(width, tileYStart)).equals(tileColor)) {
                    tileFlag = true;
                    tilesInRow++;
                    continue;
                }
            }
            if (tileFlag) {
                if (!Integer.toHexString(image.getRGB(width, tileYStart)).equals(tileColor)) {
                    if (tileXEnd == -1) {
                        tileXEnd = width;
                    }
                    tileFlag = false;
                } else {
                    image.setRGB(width, tileYStart, Color.RED.getRGB());
                }
            }
        }

        int tileSize = tileXEnd - tileXStart;
        int paddingPerTile = (image.getWidth() - (tileSize * tilesInRow)) / tilesInRow;


        BufferedImage lettersOnly = new BufferedImage(tilesInRow * tileSize - paddingPerTile * tilesInRow,
                                                tilesInRow * tileSize - paddingPerTile * tilesInRow, image.getType());
        Graphics graphics = lettersOnly.getGraphics();
        for (int row = 0; row < tilesInRow; row++) {
            for (int column = 0; column < tilesInRow; column++) {
                BufferedImage subImage = image.getSubimage(column * tileSize + (paddingPerTile * (1 + column)),
                        tileYStart + row * (tileSize + paddingPerTile), tileSize - paddingPerTile, tileSize - paddingPerTile);
                graphics.drawImage(subImage, column * tileSize - (paddingPerTile * column), row * tileSize - (paddingPerTile * row), null);
            }
        }

        // Find the word specifications

        int searchYStart = tileYStart + ((tileSize + paddingPerTile) * tilesInRow);
        String backgroundColor = conf.getString(prefixed("colors.background"));

        int boxesYStart = -1;

        outer : for (int i = searchYStart; i < image.getHeight(); i++) {
            for (int j = 0; j < image.getWidth(); j++) {
                if (!Integer.toHexString(image.getRGB(j, i)).equals(backgroundColor)) {
                    boxesYStart = i;
                    break outer;
                }
            }
        }

        if (boxesYStart == -1) {
            throw new IllegalStateException("Couldn't find the start of the boxes!");
        }

        // offset into into the boxes to get a good read on the true size
        boxesYStart += 20;

        // Logic for getting information about the boxes, such as size and distance between
        // used later
        boolean recordBorder = true;
        boolean recordSize = false;
        boolean recordBetween = false;
        int borderSize = 0;
        int insideBox = 0;
        int betweenBoxes = 0;

        for (int column = 0; column < image.getWidth(); column++) {
            if (Integer.toHexString(image.getRGB(column, boxesYStart)).equals(backgroundColor)) {
                // 2) stop recording Border
                // 3) start recording size
                if (recordBorder && borderSize > 0) {
                    recordBorder = false;
                    recordSize = true;
                }

                if (recordSize) {
                    insideBox++;
                }

                if (recordBetween) {
                    betweenBoxes++;
                }
                image.setRGB(column, boxesYStart + 1, Color.RED.getRGB());
            } else {
                image.setRGB(column, boxesYStart + 1, Color.GREEN.getRGB());
                // 1) start recording border
                if (recordBorder) {
                    borderSize++;
                }

                // 4) stop recording size
                // 5) start recording distance between boxes
                if (recordSize) {
                    recordSize = false;
                    recordBetween = true;
                }

                // 6) Done
                if (recordBetween && betweenBoxes > 0) {
                    recordBetween = false;
                }
            }
        }

        int totalBoxSize = insideBox + 2 * borderSize;

        System.out.println("Border: " + borderSize);
        System.out.println("Inside box: " + insideBox);
        System.out.println("Total box size: " + totalBoxSize);
        System.out.println("Between boxes: " + betweenBoxes);

        ArrayList<Integer> boxes = new ArrayList<Integer>();

        int boxCounter = 0;
        int spanCounter = 0;

        for (int boxRow = boxesYStart; boxRow < image.getHeight(); boxRow += totalBoxSize + 7) {
            for (int column = 0; column < image.getWidth(); column++) {
                if (!Integer.toHexString(image.getRGB(column, boxRow)).equals(backgroundColor)) {
                    boxCounter++;
                    image.setRGB(column, boxRow, Color.YELLOW.getRGB());
                    column += totalBoxSize;
                    spanCounter = 0;
                } else {
                    image.setRGB(column, boxRow, Color.BLUE.getRGB());
                    if (boxCounter > 0 && spanCounter++ > betweenBoxes) {
                        boxes.add(boxCounter);
                        spanCounter = 0;
                        boxCounter = 0;
                    }
                }
            }
            System.out.println("Boxes in row: " + boxCounter);
            boxCounter = 0;
            spanCounter = 0;
        }

        if (boxes.isEmpty()) {
            BoardProperties temp = new BoardProperties(lettersOnly, image, tileSize, tilesInRow, boxes);
            temp.saveImages(conf);
            throw new IllegalStateException("No boxes found!");
        }
        System.out.println("Found these boxes: " + boxes);

        return new BoardProperties(lettersOnly, image, tileSize, tilesInRow, boxes);
    }

    public BufferedImage getCroppedBoard(Config conf, String fileName) throws IOException {
        BufferedImage full = ImageIO.read(new File(conf.getString(prefixed("test-images-dir")) + fileName));
        double headerEnd = full.getHeight() * conf.getDouble(prefixed("header-height"));
        double footerStart = full.getHeight() - (full.getHeight() * conf.getDouble(prefixed("footer-height")));

        return full.getSubimage(0, (int)headerEnd, full.getWidth(), (int)(footerStart - headerEnd));
    }

    public String readImage(BufferedImage inputImage) {
        BytePointer output;

        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        try {
            ImageIO.write(inputImage, "png", baos);
        } catch (Exception ex) {
            System.err.println("Failed writing image to ByteStream");
            ex.printStackTrace();
        }
        PIX image = pixReadMem(baos.toByteArray(), baos.size());
        api.SetImage(image);
        // Get OCR result
        output = api.GetUTF8Text();
        String string = output.getString();

        // Destroy used object and release memory
        api.End();
        output.deallocate();
        pixDestroy(image);
        return string;
    }

    public static String prefixed(String str) {
        String prefix = "image-reader";
        return String.format("%s.%s", prefix, str);
    }
}
