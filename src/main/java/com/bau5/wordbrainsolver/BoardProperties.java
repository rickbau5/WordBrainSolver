package com.bau5.wordbrainsolver;

import com.typesafe.config.Config;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Properties;

public class BoardProperties {
    public final BufferedImage board;
    public final BufferedImage croppedImage;
    public final int tileDimension;
    public final int tilesInRow;
    public final Integer[] boxes;

    private String output = null;

    boolean directoryInitialized = false;

    BoardProperties(BufferedImage lettersImage, BufferedImage croppedImage, int dim, int numInRow, ArrayList<Integer> boxes) {
        this.board = lettersImage;
        this.croppedImage = croppedImage;
        this.tileDimension = dim;
        this.tilesInRow = numInRow;
        this.boxes = boxes.toArray(new Integer[boxes.size()]);
    }

    public void setOutput(String str) {
        if (output == null) {
            output = str;
        }
    }

    public String getOutput() {
        return output;
    }
    public int getTotalTiles() {
        return tilesInRow * tilesInRow;
    }

    public void saveImages(Config conf) throws IOException {
        String path = getOutputPath(conf);
        saveImage(board, path + "/letters-only.png");
        saveImage(croppedImage, path + "/cropped-screenshot.png");
    }

    private void saveImage(BufferedImage image, String fileName) throws IOException {
        ImageIO.write(image, "png", new File(fileName));
    }

    public void saveProperties(Config conf) throws IOException {
        String path = getOutputPath(conf);
        Properties props = new Properties();
        props.setProperty("tile-dimension", "" + tileDimension);
        props.setProperty("tiles-in-row", "" + tilesInRow);
        String output = "";
        for (Integer b : boxes) {
            output += b + " ";
        }
        props.setProperty("boxes", output);
        props.store(new FileOutputStream(getOutputPath(conf) + "/properties.txt"), null);
    }

    private String getOutputPath(Config conf) throws IOException {
        String path = conf.getString(ImageReader.prefixed("intermediary-dir"));
        if (!directoryInitialized) {
            File imagesDir = new File(path);
            if (!imagesDir.exists() && !imagesDir.mkdir()) {
                System.err.println("Failed creating intermediary directory.");
                throw new FileNotFoundException(imagesDir.getPath());
            }
            directoryInitialized = true;
        }
        return path;
    }
}
