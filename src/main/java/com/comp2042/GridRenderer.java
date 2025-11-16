package com.comp2042;

import javafx.scene.paint.Paint;
import javafx.scene.shape.Rectangle;

import java.util.function.IntFunction;

public class GridRenderer {

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
        rectangle.setArcHeight(9);
        rectangle.setArcWidth(9);
    }
}