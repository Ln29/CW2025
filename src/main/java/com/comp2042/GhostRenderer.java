package com.comp2042;

import javafx.scene.Scene;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Pane;
import javafx.scene.paint.Color;
import javafx.scene.paint.Paint;
import javafx.scene.shape.Rectangle;

import java.util.function.IntFunction;

public class GhostRenderer {

    private final BorderPane gameBoard;
    private final GridPane gamePanel;
    private final GridPane brickPanel;
    private final int brickSize;
    private final int hiddenRowCount;
    private final IntFunction<Paint> colorFunc;

    private GridPane ghostPanel;
    private Rectangle[][] ghostRectangles;

    public GhostRenderer(BorderPane gameBoard,
                         GridPane gamePanel,
                         GridPane brickPanel,
                         int brickSize,
                         int hiddenRowCount,
                         IntFunction<Paint> colorFunc) {
        this.gameBoard = gameBoard;
        this.gamePanel = gamePanel;
        this.brickPanel = brickPanel;
        this.brickSize = brickSize;
        this.hiddenRowCount = hiddenRowCount;
        this.colorFunc = colorFunc;
        init();
    }

    private void init() {
        ghostPanel = new GridPane();
        ghostPanel.setHgap(brickPanel.getHgap());
        ghostPanel.setVgap(brickPanel.getVgap());
        ghostPanel.setVisible(false);
        ghostPanel.setMouseTransparent(true);

        int maxSize = 4;
        ghostRectangles = new Rectangle[maxSize][maxSize];
        for (int i = 0; i < maxSize; i++) {
            for (int j = 0; j < maxSize; j++) {
                Rectangle rect = new Rectangle(brickSize, brickSize);
                rect.setFill(Color.TRANSPARENT);
                rect.setArcHeight(GameConstants.RECT_ARC_RADIUS);
                rect.setArcWidth(GameConstants.RECT_ARC_RADIUS);
                ghostRectangles[i][j] = rect;
                ghostPanel.add(rect, j, i);
            }
        }
    }

    public void addToScene(Scene scene) {
        if (scene == null) return;
        Pane root = (Pane) scene.getRoot();
        root.getChildren().add(ghostPanel);
        ghostPanel.toBack();
        brickPanel.toFront();
    }

    public void render(GhostBrick ghostBrick, boolean boardCentered) {
        if (ghostBrick == null) {
            ghostPanel.setVisible(false);
            return;
        }

        if (boardCentered && gameBoard.getLayoutX() > 0) {
            double x = gameBoard.getLayoutX() + gamePanel.getLayoutX() + ghostBrick.getxPosition() * (ghostPanel.getVgap() + brickSize);
            double cellSize = ghostPanel.getHgap() + brickSize;
            double y = gameBoard.getLayoutY() + gamePanel.getLayoutY() + (ghostBrick.getyPosition() - hiddenRowCount) * cellSize;

            ghostPanel.setLayoutX(x);
            ghostPanel.setLayoutY(y);
            ghostPanel.setVisible(true);
            ghostPanel.toBack();
        } else {
            ghostPanel.setVisible(false);
            return;
        }

        // Clear existing ghost cells
        for (int i = 0; i < ghostRectangles.length; i++) {
            for (int j = 0; j < ghostRectangles[i].length; j++) {
                ghostRectangles[i][j].setFill(Color.TRANSPARENT);
                ghostRectangles[i][j].setStroke(null);
            }
        }

        int[][] ghostData = ghostBrick.getBrickData();
        int ghostY = ghostBrick.getyPosition();
        for (int i = 0; i < ghostData.length && i < ghostRectangles.length; i++) {
            for (int j = 0; j < ghostData[i].length && j < ghostRectangles[i].length; j++) {
                int cell = ghostData[i][j];
                int boardRow = ghostY + i;
                if (boardRow >= hiddenRowCount && cell != 0) {
                    ghostRectangles[i][j].setFill(Color.TRANSPARENT);
                    Paint stroke = toGhostStroke(colorFunc.apply(cell));
                    ghostRectangles[i][j].setStroke(stroke);
                    ghostRectangles[i][j].setStrokeWidth(2.0);
                    ghostRectangles[i][j].setArcHeight(9);
                    ghostRectangles[i][j].setArcWidth(9);
                }
            }
        }
    }

    private Paint toGhostStroke(Paint base) {
        if (base == Color.TRANSPARENT) return Color.TRANSPARENT;
        if (base instanceof Color) {
            Color c = (Color) base;
            return new Color(c.getRed(), c.getGreen(), c.getBlue(), 0.8);
        }
        return base;
    }
}