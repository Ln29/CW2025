package com.comp2042.ui.util;

import javafx.scene.Scene;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.Pane;

/**
 * Utility class for accessing scene and root pane from UI components.
 * Provides safe access to scene hierarchy.
 */
public final class SceneAccessor {
    private SceneAccessor() {}

    /**
     * Gets the scene containing the specified game board.
     * 
     * @param gameBoard the game board
     * @return the scene, or null if not attached
     */
    public static Scene sceneOf(BorderPane gameBoard) {
        if (gameBoard == null) return null;
        return gameBoard.getScene();
    }

    /**
     * Gets the root pane of the scene containing the game board.
     * 
     * @param gameBoard the game board
     * @return the root pane, or null if scene not available
     */
    public static Pane rootOf(BorderPane gameBoard) {
        Scene scene = sceneOf(gameBoard);
        if (scene == null) return null;
        return (Pane) scene.getRoot();
    }
}

