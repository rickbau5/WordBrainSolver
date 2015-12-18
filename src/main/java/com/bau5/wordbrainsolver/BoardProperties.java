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

    private String tesseractOutput = "";
    private String output = null;

    boolean directoryInitialized = false;

    public BoardProperties(BufferedImage lettersImage, BufferedImage croppedImage, int dim, int numInRow,
                           ArrayList<Integer> boxes) {
        this.board = lettersImage;
        this.croppedImage = croppedImage;
        this.tileDimension = dim;
        this.tilesInRow = numInRow;
        this.boxes = boxes.toArray(new Integer[boxes.size()]);
    }

    public void setTesseractOutput(String str) {
        if (tesseractOutput.equals("")) {
            tesseractOutput = str;
        }
    }

    public String getTesseractOutput() {
        return tesseractOutput;
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
        Properties props = new Properties();
        props.setProperty("tile-dimension", "" + tileDimension);
        props.setProperty("tiles-in-row", "" + tilesInRow);
        props.setProperty("tesseract-output", tesseractOutput != null ? tesseractOutput : "null");
        props.setProperty("processed-output", output != null ? output : "null");

        String boxesString = "";
        for (Integer b : boxes) {
            boxesString += b + " ";
        }
        props.setProperty("boxes", boxesString);

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
