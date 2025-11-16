package com.comp2042;

import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.Pane;

public class MenuManager {

    private final Pane rootPane;
    private final BorderPane gameBoard;

    public MenuManager(Pane rootPane, BorderPane gameBoard) {
        this.rootPane = rootPane;
        this.gameBoard = gameBoard;
    }

    public void ensureOnTop(Node overlay) {
        if (rootPane.getChildren().contains(overlay)) {
            rootPane.getChildren().remove(overlay);
        }
        rootPane.getChildren().add(overlay);
        overlay.toFront();
    }

    public void centerOnScene(Node overlay, Scene scene) {
        double sceneWidth = scene.getWidth();
        double sceneHeight = scene.getHeight();
        overlay.setLayoutX((sceneWidth - overlay.prefWidth(-1)) / 2);
        overlay.setLayoutY((sceneHeight - overlay.prefHeight(-1)) / 2);
    }

    public void centerOnBoard(Node overlay) {
        double boardWidth = GameConstants.BOARD_COLS * GameConstants.BRICK_SIZE + GameConstants.BOARD_BORDER_TOTAL_PX;
        double boardHeight = GameConstants.BOARD_VISIBLE_ROWS * GameConstants.BRICK_SIZE + GameConstants.BOARD_BORDER_TOTAL_PX;
        double boardCenterX = gameBoard.getLayoutX() + boardWidth / 2.0;
        double boardCenterY = gameBoard.getLayoutY() + boardHeight / 2.0;
        overlay.setLayoutX(boardCenterX - overlay.prefWidth(-1) / 2);
        overlay.setLayoutY(boardCenterY - overlay.prefHeight(-1) / 2);
    }

    public void show(Node overlay) {
        overlay.setVisible(true);
        ensureOnTop(overlay);
    }

    public void showCenteredOnScene(Node overlay, Scene scene) {
        show(overlay);
        centerOnScene(overlay, scene);
    }

    public void showCenteredOnBoard(Node overlay) {
        show(overlay);
        centerOnBoard(overlay);
    }

    public void hide(Node overlay) {
        overlay.setVisible(false);
    }
}