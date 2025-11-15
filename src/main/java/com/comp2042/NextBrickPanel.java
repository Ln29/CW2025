package com.comp2042;

import com.comp2042.logic.bricks.Brick;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.control.Label;
import javafx.scene.layout.*;
import javafx.scene.paint.Color;
import javafx.scene.paint.Paint;
import javafx.scene.shape.Rectangle;

import java.util.List;

public class NextBrickPanel extends VBox {

    private static final int BRICK_SIZE = 15;
    private static final int BRICKS_TO_SHOW = 5;
    private static final int PANEL_WIDTH = 100;
    private static final int BRICK_PANEL_SIZE = 40;

    private final VBox[] brickContainers;
    private final GridPane[] brickGrids;

    public NextBrickPanel() {
        setAlignment(Pos.TOP_CENTER);
        setSpacing(5);
        setPadding(new Insets(15, 10, 15, 10));
        setPrefWidth(PANEL_WIDTH);
        setMinWidth(PANEL_WIDTH);
        setMaxWidth(PANEL_WIDTH);

        // Border order: trbl
        setStyle("-fx-border-color: linear-gradient(#2A5058, #61a2b1); " +
                "-fx-border-width: 12px 12px 12px 0; " + // left is hidden
                "-fx-border-radius: 12px 12px 12px 0; " + // Rounded corners
                "-fx-background-color: rgba(56, 52, 52, 0.52);");

        // Add "NEXT" label at the top
        Label nextLabel = new Label("NEXT");
        nextLabel.setTextFill(Color.YELLOW);
        nextLabel.setFont(javafx.scene.text.Font.font("Let's go Digital", javafx.scene.text.FontWeight.BOLD, 22));
        nextLabel.setPadding(new Insets(0, 0, 12, 0));
        getChildren().add(nextLabel);

        brickContainers = new VBox[BRICKS_TO_SHOW];
        brickGrids = new GridPane[BRICKS_TO_SHOW];

        // Create container for each of the 5 next bricks
        for (int i = 0; i < BRICKS_TO_SHOW; i++) {
            VBox container = new VBox();
            container.setAlignment(Pos.CENTER);
            container.setPrefHeight(BRICK_PANEL_SIZE);
            container.setMinHeight(BRICK_PANEL_SIZE);
            container.setMaxHeight(BRICK_PANEL_SIZE);

            GridPane grid = new GridPane();
            grid.setHgap(1);
            grid.setVgap(1);
            grid.setAlignment(Pos.CENTER);

            container.getChildren().add(grid);
            brickContainers[i] = container;
            brickGrids[i] = grid;
            getChildren().add(container);
        }
    }

    public void updateBricks(List<Brick> nextBricks) {
        // Clear all grid first
        for (GridPane grid : brickGrids) {
            grid.getChildren().clear();
        }

        // Update each grid with the corresponding brick
        int bricksToDisplay = Math.min(BRICKS_TO_SHOW, nextBricks.size());
        for (int i = 0; i < bricksToDisplay; i++) {
            Brick brick = nextBricks.get(i);
            if (brick != null) {
                displayBrick(brick, brickGrids[i]);
            }
        }
    }

    private void displayBrick(Brick brick, GridPane grid) {
        if (brick == null || brick.getShapeMatrix().isEmpty()) {
            return;
        }

        int[][] shape = brick.getShapeMatrix().get(0); // Get first rotation
        int rows = shape.length;
        int cols = shape[0].length;

        // Calculate offset to center the brick in the grid
        int maxSize = Math.max(rows, cols);
        int offsetX = (maxSize - cols) / 2;
        int offsetY = (maxSize - rows) / 2;

        // Create rectangles for each cell in the brick
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                if (shape[i][j] != 0) {
                    Rectangle rect = new Rectangle(BRICK_SIZE, BRICK_SIZE);
                    rect.setFill(getFillColor(shape[i][j]));
                    rect.setArcHeight(9);
                    rect.setArcWidth(9);
                    grid.add(rect, j + offsetX, i + offsetY);
                }
            }
        }
    }

    private Paint getFillColor(int colorCode) {
        switch (colorCode) {
            case 0:
                return Color.TRANSPARENT;
            case 1:
                return Color.AQUA;
            case 2:
                return Color.BLUEVIOLET;
            case 3:
                return Color.DARKGREEN;
            case 4:
                return Color.YELLOW;
            case 5:
                return Color.RED;
            case 6:
                return Color.BEIGE;
            case 7:
                return Color.BURLYWOOD;
            default:
                return Color.WHITE;
        }
    }

    public void position(double boardX, double boardY, double boardWidth, double boardHeight) {
        double panelX = boardX + boardWidth + 10;
        double panelY = boardY;
        double panelHeight = boardHeight / 2;

        setLayoutX(panelX);
        setLayoutY(panelY);
        setPrefHeight(panelHeight);
        setMinHeight(panelHeight);
        setMaxHeight(panelHeight);
    }
}