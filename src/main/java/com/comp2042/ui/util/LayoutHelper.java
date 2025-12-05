package com.comp2042.ui.util;

import com.comp2042.config.GameConstants;
import javafx.scene.Scene;
import javafx.scene.layout.BorderPane;

/**
 * Utility class for layout operations.
 * Provides helper methods for positioning UI elements.
 */
public final class LayoutHelper {

    private LayoutHelper() {}

    /**
     * Centers the game board on the scene.
     * 
     * @param scene the scene containing the game board
     * @param gameBoard the game board to center
     */
    public static void centerGameBoard(Scene scene, BorderPane gameBoard) {
        double boardWidth = GameConstants.BOARD_COLS * GameConstants.BRICK_SIZE + GameConstants.BOARD_BORDER_TOTAL_PX;
        double boardHeight = GameConstants.BOARD_VISIBLE_ROWS * GameConstants.BRICK_SIZE + GameConstants.BOARD_BORDER_TOTAL_PX;
        double centerX = (scene.getWidth() - boardWidth) / 2;
        double centerY = (scene.getHeight() - boardHeight) / 2;
        gameBoard.setLayoutX(centerX);
        gameBoard.setLayoutY(centerY);
    }
}

