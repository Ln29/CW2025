package com.comp2042;

import javafx.scene.Scene;
import javafx.scene.layout.BorderPane;

public final class LayoutHelper {

    private LayoutHelper() {}

    public static void centerGameBoard(Scene scene, BorderPane gameBoard) {
        double boardWidth = GameConstants.BOARD_COLS * GameConstants.BRICK_SIZE + GameConstants.BOARD_BORDER_TOTAL_PX;
        double boardHeight = GameConstants.BOARD_VISIBLE_ROWS * GameConstants.BRICK_SIZE + GameConstants.BOARD_BORDER_TOTAL_PX;
        double centerX = (scene.getWidth() - boardWidth) / 2;
        double centerY = (scene.getHeight() - boardHeight) / 2;
        gameBoard.setLayoutX(centerX);
        gameBoard.setLayoutY(centerY);
    }
}