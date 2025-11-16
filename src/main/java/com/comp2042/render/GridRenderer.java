package com.comp2042.render;

import com.comp2042.config.GameConstants;
import javafx.scene.layout.GridPane;
import javafx.scene.paint.Color;
import javafx.scene.paint.Paint;
import javafx.scene.shape.Line;
import javafx.scene.shape.Rectangle;

import java.util.function.IntFunction;

public class GridRenderer {

    public void drawGridLines(GridPane gamePanel, int brickSize) {
        Color gridColor = Color.rgb(128, 128, 128, 0.5);

        double hgap = gamePanel.getHgap();
        double vgap = gamePanel.getVgap();
        double cellWidth = brickSize + hgap;
        double cellHeight = brickSize + vgap;
        double totalWidth = GameConstants.BOARD_COLS * brickSize + (GameConstants.BOARD_COLS - 1) * hgap;
        double totalHeight = GameConstants.BOARD_VISIBLE_ROWS * brickSize + (GameConstants.BOARD_VISIBLE_ROWS - 1) * vgap;

        // Vertical lines
        for (int i = 0; i <= GameConstants.BOARD_COLS; i++) {
            Line verticalLine = new Line();
            double x = i * cellWidth;
            verticalLine.setStartX(x);
            verticalLine.setStartY(0);
            verticalLine.setEndX(x);
            verticalLine.setEndY(totalHeight);
            verticalLine.setStroke(gridColor);
            verticalLine.setStrokeWidth(GameConstants.GRID_STROKE_WIDTH);
            verticalLine.setManaged(false);
            gamePanel.getChildren().add(0, verticalLine);
            verticalLine.toBack();
        }

        // Horizontal lines
        for (int i = 0; i <= GameConstants.BOARD_VISIBLE_ROWS; i++) {
            Line horizontalLine = new Line();
            double y = i * cellHeight;
            horizontalLine.setStartX(0);
            horizontalLine.setStartY(y);
            horizontalLine.setEndX(totalWidth);
            horizontalLine.setEndY(y);
            horizontalLine.setStroke(gridColor);
            horizontalLine.setStrokeWidth(GameConstants.GRID_STROKE_WIDTH);
            horizontalLine.setManaged(false);
            gamePanel.getChildren().add(0, horizontalLine);
            horizontalLine.toBack();
        }
    }

    public void refreshGameBackground(int[][] board, Rectangle[][] displayMatrix, IntFunction<Paint> colorFunction, int hiddenRowCount) {
        if (board == null || displayMatrix == null) return;
        int maxRow = Math.min(board.length - 1, hiddenRowCount + GameConstants.BOARD_VISIBLE_ROWS);
        for (int i = hiddenRowCount; i <= maxRow; i++) {
            for (int j = 0; j < board[i].length; j++) {
                setRectangleData(board[i][j], displayMatrix[i][j], colorFunction);
            }
        }
    }

    public void setRectangleData(int color, Rectangle rectangle, IntFunction<Paint> colorFunction) {
        if (rectangle == null) return;
        rectangle.setFill(colorFunction.apply(color));
        rectangle.setArcHeight(GameConstants.RECT_ARC_RADIUS);
        rectangle.setArcWidth(GameConstants.RECT_ARC_RADIUS);
    }
}

