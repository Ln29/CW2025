package com.comp2042.ui.panels;

import com.comp2042.config.GameConstants;
import com.comp2042.core.ViewData;
import javafx.scene.Scene;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.GridPane;

/**
 * Utility class for positioning UI panels relative to the game board.
 */
public class PanelPositioner {

    private final BorderPane gameBoard;

    public PanelPositioner(BorderPane gameBoard) {
        this.gameBoard = gameBoard;
    }

    public void positionNextBrickPanel(NextBrickPanel nextBrickPanel, Scene scene) {
        if (nextBrickPanel == null) return;
        double boardWidth = GameConstants.BOARD_COLS * GameConstants.BRICK_SIZE + GameConstants.BOARD_BORDER_TOTAL_PX;
        double boardHeight = GameConstants.BOARD_VISIBLE_ROWS * GameConstants.BRICK_SIZE + GameConstants.BOARD_BORDER_TOTAL_PX;
        double boardX = gameBoard.getLayoutX();
        double boardY = gameBoard.getLayoutY();
        nextBrickPanel.position(boardX, boardY, boardWidth, boardHeight);
    }

    public void positionHoldBrickPanel(HoldBrickPanel holdBrickPanel, Scene scene) {
        if (holdBrickPanel == null) return;
        double boardWidth = GameConstants.BOARD_COLS * GameConstants.BRICK_SIZE + GameConstants.BOARD_BORDER_TOTAL_PX;
        double boardHeight = GameConstants.BOARD_VISIBLE_ROWS * GameConstants.BRICK_SIZE + GameConstants.BOARD_BORDER_TOTAL_PX;
        double boardX = gameBoard.getLayoutX();
        double boardY = gameBoard.getLayoutY();
        holdBrickPanel.position(boardX, boardY, boardWidth, boardHeight);
    }

    public void positionStatsPanel(StatsPanel statsPanel, Scene scene) {
        if (statsPanel == null) return;
        double boardWidth = GameConstants.BOARD_COLS * GameConstants.BRICK_SIZE + GameConstants.BOARD_BORDER_TOTAL_PX;
        double boardHeight = GameConstants.BOARD_VISIBLE_ROWS * GameConstants.BRICK_SIZE + GameConstants.BOARD_BORDER_TOTAL_PX;
        double boardX = gameBoard.getLayoutX();
        double boardY = gameBoard.getLayoutY();
        statsPanel.position(boardX, boardY, boardWidth, boardHeight);
    }

    public void positionStatsPanelRight(RightStatsPanel statsPanelRight, Scene scene) {
        if (statsPanelRight == null) return;
        double boardWidth = GameConstants.BOARD_COLS * GameConstants.BRICK_SIZE + GameConstants.BOARD_BORDER_TOTAL_PX;
        double boardHeight = GameConstants.BOARD_VISIBLE_ROWS * GameConstants.BRICK_SIZE + GameConstants.BOARD_BORDER_TOTAL_PX;
        double boardX = gameBoard.getLayoutX();
        double boardY = gameBoard.getLayoutY();
        statsPanelRight.position(boardX, boardY, boardWidth, boardHeight);
    }

    public void positionBrickPanel(GridPane brickPanel, GridPane gamePanel, ViewData brick) {
        if (brickPanel == null || gamePanel == null || brick == null) return;
        double x = gameBoard.getLayoutX() + gamePanel.getLayoutX() + brick.getxPosition() * (brickPanel.getVgap() + GameConstants.BRICK_SIZE);
        double cellSize = brickPanel.getHgap() + GameConstants.BRICK_SIZE;
        double y = gameBoard.getLayoutY() + gamePanel.getLayoutY() + (brick.getyPosition() - GameConstants.HIDDEN_ROW_COUNT) * cellSize;
        brickPanel.setLayoutX(x);
        brickPanel.setLayoutY(y);
    }
}


