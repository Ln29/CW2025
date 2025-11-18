package com.comp2042.render;

import com.comp2042.core.Board;
import com.comp2042.core.ViewData;
import com.comp2042.ui.Ui;
import com.comp2042.ui.panels.PanelManager;
import com.comp2042.ui.panels.PanelPositioner;
import javafx.beans.property.BooleanProperty;
import javafx.scene.Scene;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.GridPane;
import javafx.scene.paint.Color;
import javafx.scene.paint.Paint;
import javafx.scene.shape.Rectangle;

import java.util.function.IntFunction;
import java.util.function.Supplier;

/**
 * View-only renderer responsible for brick rendering, background refresh, and display updates.
 */
public class GameRenderer {

    private final BorderPane gameBoard;
    private final GridPane gamePanel;
    private final GridPane brickPanel;
    private final int BRICK_SIZE;
    private final int HIDDEN_ROW_COUNT;
    private final IntFunction<Paint> colorProvider;
    private final BooleanProperty isPause;
    private final Supplier<Board> boardSupplier;

    private PanelManager panelManager;

    private Rectangle[][] rectangles;
    private Rectangle[][] displayMatrix;
    private ViewData initialBrickData = null;
    private boolean boardCentered = false;
    private final GridRenderer gridRenderer = new GridRenderer();
    private GhostRenderer ghostRenderer;

    public GameRenderer(
            BorderPane gameBoard,
            GridPane gamePanel,
            GridPane brickPanel,
            int brickSize,
            int hiddenRowCount,
            IntFunction<Paint> colorProvider,
            BooleanProperty isPause,
            Supplier<Board> boardSupplier,
            PanelManager panelManager
    ) {
        this.gameBoard = gameBoard;
        this.gamePanel = gamePanel;
        this.brickPanel = brickPanel;
        this.BRICK_SIZE = brickSize;
        this.HIDDEN_ROW_COUNT = hiddenRowCount;
        this.colorProvider = colorProvider;
        this.isPause = isPause;
        this.boardSupplier = boardSupplier;
        this.panelManager = panelManager;
    }

    public void setPanelManager(PanelManager panelManager) {
        this.panelManager = panelManager;
    }

    public void setBoardCentered(boolean boardCentered) {
        this.boardCentered = boardCentered;
    }

    public void refreshAfterCenter() {
        if (initialBrickData != null && rectangles != null && rectangles.length > 0) {
            refreshBrick(initialBrickData);
        }
    }

    public void initGameView(int[][] boardMatrix, ViewData brick) {
        // Initialize display matrix (visible rows only)
        displayMatrix = new Rectangle[boardMatrix.length][boardMatrix[0].length];
        for (int i = HIDDEN_ROW_COUNT; i <= 21 && i < boardMatrix.length; i++) {
            for (int j = 0; j < boardMatrix[i].length; j++) {
                Rectangle rectangle = new Rectangle(BRICK_SIZE, BRICK_SIZE);
                rectangle.setFill(Color.TRANSPARENT);
                displayMatrix[i][j] = rectangle;
                gamePanel.add(rectangle, j, i - HIDDEN_ROW_COUNT);
            }
        }

        // Initialize brick rectangles
        rectangles = new Rectangle[brick.getBrickData().length][brick.getBrickData()[0].length];
        for (int i = 0; i < brick.getBrickData().length; i++) {
            for (int j = 0; j < brick.getBrickData()[i].length; j++) {
                Rectangle rectangle = new Rectangle(BRICK_SIZE, BRICK_SIZE);
                rectangle.setFill(colorProvider.apply(brick.getBrickData()[i][j]));
                rectangles[i][j] = rectangle;
                brickPanel.add(rectangle, j, i);
            }
        }
        initialBrickData = brick;

        // Initialize ghost renderer
        ghostRenderer = new GhostRenderer(gameBoard, gamePanel, brickPanel, BRICK_SIZE, HIDDEN_ROW_COUNT, colorProvider::apply);
        Ui.run(() -> {
            Scene scene = gameBoard.getScene();
            if (scene != null) {
                ghostRenderer.addToScene(scene);
            }
        });
        gridRenderer.drawGridLines(gamePanel, BRICK_SIZE);
    }

    public void refreshBrick(ViewData brick) {
        if (isPause.getValue() == Boolean.FALSE) {
            // Position brick panel only when board is centered
            if (boardCentered && gameBoard.getLayoutX() > 0) {
                positionBrickPanel(brick);
                brickPanel.setVisible(true);
            } else {
                brickPanel.setVisible(false);
            }

            // Only show cells in visible rows
            int brickY = brick.getyPosition();
            for (int i = 0; i < brick.getBrickData().length; i++) {
                for (int j = 0; j < brick.getBrickData()[i].length; j++) {
                    int cellValue = brick.getBrickData()[i][j];
                    int boardRow = brickY + i;
                    if (boardRow >= HIDDEN_ROW_COUNT) {
                        setRectangleData(cellValue, rectangles[i][j]);
                    } else {
                        rectangles[i][j].setFill(Color.TRANSPARENT);
                    }
                }
            }

            Board board = boardSupplier.get();
            if (ghostRenderer != null && board != null) {
                ghostRenderer.render(board.getGhostBrick(), boardCentered);
            }
        }
    }

    public void postMoveRefresh(ViewData viewData) {
        refreshBrick(viewData);
        if (panelManager != null) panelManager.updateNextBrickPanel();
        if (panelManager != null) panelManager.updateStatsPanels();
    }

    public void refreshGameBackground(int[][] board) {
        if (gridRenderer != null) {
            gridRenderer.refreshGameBackground(board, displayMatrix, colorProvider::apply, HIDDEN_ROW_COUNT);
        }
    }

    private void setRectangleData(int color, Rectangle rectangle) {
        if (gridRenderer != null) {
            gridRenderer.setRectangleData(color, rectangle, colorProvider::apply);
        }
    }

    private void positionBrickPanel(ViewData brick) {
        new PanelPositioner(gameBoard).positionBrickPanel(brickPanel, gamePanel, brick);
    }
}

