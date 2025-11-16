package com.comp2042.ui;

import javafx.scene.Scene;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.Pane;

public final class SceneAccessor {
    private SceneAccessor() {}

    public static Scene sceneOf(BorderPane gameBoard) {
        if (gameBoard == null) return null;
        return gameBoard.getScene();
    }

    public static Pane rootOf(BorderPane gameBoard) {
        Scene scene = sceneOf(gameBoard);
        if (scene == null) return null;
        return (Pane) scene.getRoot();
    }
}


