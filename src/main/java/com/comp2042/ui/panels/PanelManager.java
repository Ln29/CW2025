package com.comp2042.ui.panels;

import com.comp2042.controller.GameModeController;
import com.comp2042.controller.GameState;
import com.comp2042.core.Board;
import com.comp2042.ui.SceneAccessor;
import com.comp2042.ui.Ui;
import com.comp2042.config.StatsUpdater;
import javafx.scene.Group;
import javafx.scene.Scene;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.Pane;

public class PanelManager {
    private final BorderPane gameBoard;
    private final Board board;
    private final StatsUpdater statsUpdater;
    private final GameState gameState;
    private final PanelPositioner panelPositioner;
    private GameModeController gameModeController;

    private NextBrickPanel nextBrickPanel;
    private HoldBrickPanel holdBrickPanel;
    private StatsPanel statsPanel;
    private StatsPanelRight statsPanelRight;

    public PanelManager(BorderPane gameBoard, Board board, StatsUpdater statsUpdater, GameState gameState) {
        this.gameBoard = gameBoard;
        this.board = board;
        this.statsUpdater = statsUpdater;
        this.gameState = gameState;
        this.panelPositioner = new PanelPositioner(gameBoard);
    }

    public void setGameModeController(GameModeController gameModeController) {
        this.gameModeController = gameModeController;
    }

    // Next Panel
    public void initializeNextBrickPanel(Scene scene) {
        if (nextBrickPanel == null) {
            nextBrickPanel = new NextBrickPanel();
            Ui.run(() -> {
                javafx.scene.Group gameplayLayer = findGameplayLayer();
                if (gameplayLayer != null) {
                    gameplayLayer.getChildren().add(nextBrickPanel);
                    positionNextBrickPanel(scene);
                    updateNextBrickPanel();
                }
            });
        } else {
            updateNextBrickPanel();
        }
    }

    public void positionNextBrickPanel(Scene scene) {
        if (nextBrickPanel == null) return;
        panelPositioner.positionNextBrickPanel(nextBrickPanel, scene);
    }

    public void updateNextBrickPanel() {
        if (nextBrickPanel != null && board != null) {
            nextBrickPanel.updateBricks(board.getNextBricks(5));
        }
    }

    // Hold Panel
    public void initializeHoldBrickPanel(Scene scene) {
        if (holdBrickPanel == null) {
            holdBrickPanel = new HoldBrickPanel();
            Ui.run(() -> {
                javafx.scene.Group gameplayLayer = findGameplayLayer();
                if (gameplayLayer != null) {
                    gameplayLayer.getChildren().add(holdBrickPanel);
                    positionHoldBrickPanel(scene);
                    updateHoldBrickPanel();
                }
            });
        } else {
            updateHoldBrickPanel();
        }
    }

    public void positionHoldBrickPanel(Scene scene) {
        if (holdBrickPanel == null) return;
        panelPositioner.positionHoldBrickPanel(holdBrickPanel, scene);
    }

    public void updateHoldBrickPanel() {
        if (holdBrickPanel != null && board != null) {
            holdBrickPanel.updateBrick(board.getHeldBrick());
        }
    }

    // Stats Left
    public void initializeStatsPanel(Scene scene) {
        if (statsPanel == null) {
            statsPanel = new StatsPanel();
            Ui.run(() -> {
                javafx.scene.Group gameplayLayer = findGameplayLayer();
                if (gameplayLayer != null) {
                    gameplayLayer.getChildren().add(statsPanel);
                    positionStatsPanel(scene);
                    updateStatsPanels();
                }
            });
        } else {
            updateStatsPanels();
        }
    }

    private Group findGameplayLayer() {
        Pane root = SceneAccessor.rootOf(gameBoard);
        if (root == null) return null;
        for (javafx.scene.Node child : root.getChildren()) {
            if (child instanceof Group) {
                Group group = (Group) child;
                if (group.getChildren().contains(gameBoard)) {
                    return group;
                }
            }
        }
        return null;
    }

    public void positionStatsPanel(Scene scene) {
        if (statsPanel == null) return;
        panelPositioner.positionStatsPanel(statsPanel, scene);
    }

    // Stats Right
    public void initializeStatsPanelRight(Scene scene) {
        if (statsPanelRight == null) {
            statsPanelRight = new StatsPanelRight();
            Ui.run(() -> {
                javafx.scene.Group gameplayLayer = findGameplayLayer();
                if (gameplayLayer != null) {
                    statsPanelRight.addToGameplayLayer(gameplayLayer);
                } else {
                    statsPanelRight.addToScene(scene);
                }
                positionStatsPanelRight(scene);
                updateStatsPanels();
            });
        } else {
            updateStatsPanels();
        }
    }

    public void positionStatsPanelRight(Scene scene) {
        if (statsPanelRight == null) return;
        panelPositioner.positionStatsPanelRight(statsPanelRight, scene);
    }

    // Combined Stats update
    public void updateStatsPanels() {
        statsUpdater.updateAllStats(gameState, board, statsPanel, statsPanelRight, gameModeController);
    }

    // Expose for timer updates
    public StatsPanel getStatsPanel() {
        return statsPanel;
    }
}


