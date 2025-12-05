package com.comp2042.ui;

import com.comp2042.core.Board;
import com.comp2042.core.ViewData;
import com.comp2042.ui.panels.PanelManager;
import com.comp2042.ui.panels.PanelPositioner;
import com.comp2042.ui.util.PlatformUtils;
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

    /**
     * Creates a game renderer with specified components.
     * 
     * @param gameBoard main game board container
     * @param gamePanel grid pane for board cells
     * @param brickPanel grid pane for active brick
     * @param brickSize size of each brick cell in pixels
     * @param hiddenRowCount number of hidden rows at top
     * @param colorProvider function to get color for brick values
     * @param isPause pause state property
     * @param boardSupplier supplier for board instance
     * @param panelManager panel manager instance
     */
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

    /**
     * Sets the panel manager for updating UI panels.
     * 
     * @param panelManager panel manager instance
     */
    public void setPanelManager(PanelManager panelManager) {
        this.panelManager = panelManager;
    }

    /**
     * Sets whether the board is centered.
     * 
     * @param boardCentered true if board is centered
     */
    public void setBoardCentered(boolean boardCentered) {
        this.boardCentered = boardCentered;
    }

    /**
     * Refreshes brick display after board centering.
     */
    public void refreshAfterCenter() {
        if (initialBrickData != null && rectangles != null && rectangles.length > 0) {
            refreshBrick(initialBrickData);
        }
    }

    /**
     * Initializes the game view with board matrix and initial brick.
     * 
     * @param boardMatrix initial board state
     * @param brick initial brick view data
     */
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
        PlatformUtils.run(() -> {
            Scene scene = gameBoard.getScene();
            if (scene != null) {
                ghostRenderer.addToScene(scene);
            }
        });
        gridRenderer.drawGridLines(gamePanel, BRICK_SIZE);
    }

    /**
     * Refreshes the active brick display.
     * 
     * @param brick current brick view data
     */
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

    /**
     * Refreshes display after a move, including brick and panels.
     * 
     * @param viewData updated view data
     */
    public void postMoveRefresh(ViewData viewData) {
        refreshBrick(viewData);
        if (panelManager != null) panelManager.updateNextBrickPanel();
        if (panelManager != null) panelManager.updateStatsPanels();
    }

    /**
     * Refreshes the game background with updated board state.
     * 
     * @param board updated board matrix
     */
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

