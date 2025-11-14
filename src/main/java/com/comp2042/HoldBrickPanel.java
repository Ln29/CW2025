package com.comp2042;

import com.comp2042.logic.bricks.Brick;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.control.Label;
import javafx.scene.layout.*;
import javafx.scene.paint.Color;
import javafx.scene.paint.Paint;
import javafx.scene.shape.Rectangle;

public class HoldBrickPanel extends VBox {

    private static final int BRICK_SIZE = 15;
    private static final int PANEL_WIDTH = 95;
    private static final int BRICK_PANEL_SIZE = 80;

    private GridPane brickGrid;
    private Label holdLabel;

    public HoldBrickPanel() {
        setAlignment(Pos.TOP_CENTER);
        setSpacing(10);
        setPadding(new Insets(1, 10, 15, 10));
        setPrefWidth(PANEL_WIDTH);
        setMinWidth(PANEL_WIDTH);
        setMaxWidth(PANEL_WIDTH);

        // Border order: t,r,b,l
        setStyle("-fx-border-color: linear-gradient(#2A5058, #61a2b1); " +
                "-fx-border-width: 12px 0 12px 12px; " + // right is hidden
                "-fx-border-radius: 12px 12px 0 12px; " + // Rounded corners
                "-fx-background-color: rgba(56, 52, 52, 0.50);");

        // Add "HOLD" label at the top
        holdLabel = new Label("HOLD");
        holdLabel.setTextFill(Color.YELLOW);
        holdLabel.setFont(javafx.scene.text.Font.font("Let's go Digital", javafx.scene.text.FontWeight.BOLD, 22));
        holdLabel.setPadding(new Insets(15, 0, 0, 0));
        getChildren().add(holdLabel);

        // Create container for the held brick
        VBox container = new VBox();
        container.setAlignment(Pos.CENTER);
        container.setPrefHeight(BRICK_PANEL_SIZE);
        container.setMinHeight(BRICK_PANEL_SIZE);
        container.setMaxHeight(BRICK_PANEL_SIZE);

        brickGrid = new GridPane();
        brickGrid.setHgap(1);
        brickGrid.setVgap(1);
        brickGrid.setAlignment(Pos.CENTER);

        container.getChildren().add(brickGrid);
        getChildren().add(container);
    }

    public void updateBrick(Brick brick) {
        brickGrid.getChildren().clear();

        if (brick != null && !brick.getShapeMatrix().isEmpty()) {
            displayBrick(brick, brickGrid);
        }
    }

    private void displayBrick(Brick brick, GridPane grid) {
        if (brick == null || brick.getShapeMatrix().isEmpty()) {
            return;
        }

        int[][] shape = brick.getShapeMatrix().get(0);
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
}
