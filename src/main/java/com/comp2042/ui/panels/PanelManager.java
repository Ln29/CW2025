package com.comp2042.ui.panels;

import com.comp2042.controller.GameModeController;
import com.comp2042.controller.GameState;
import com.comp2042.core.Board;
import com.comp2042.ui.util.SceneAccessor;
import com.comp2042.ui.util.PlatformUtils;
import javafx.scene.Group;
import javafx.scene.Scene;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.Pane;

/**
 * Manages all game UI panels including next brick, hold brick, and stats panels.
 * Handles panel initialization, positioning, and updates.
 */
public class PanelManager {
    private final BorderPane gameBoard;
    private final Board board;
    private final GameState gameState;
    private final PanelPositioner panelPositioner;
    private GameModeController gameModeController;

    private NextBrickPanel nextBrickPanel;
    private HoldBrickPanel holdBrickPanel;
    private StatsPanel statsPanel;
    private RightStatsPanel statsPanelRight;

    public PanelManager(BorderPane gameBoard, Board board, GameState gameState) {
        this.gameBoard = gameBoard;
        this.board = board;
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
            PlatformUtils.run(() -> {
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
            PlatformUtils.run(() -> {
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
            PlatformUtils.run(() -> {
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
            statsPanelRight = new RightStatsPanel();
            PlatformUtils.run(() -> {
                javafx.scene.Group gameplayLayer = findGameplayLayer();
                if (gameplayLayer != null) {
                    gameplayLayer.getChildren().add(statsPanelRight);
                } else {
                    // Fallback - should not happen in normal flow
                    Pane rootPane = (Pane) scene.getRoot();
                    if (rootPane != null) {
                        rootPane.getChildren().add(statsPanelRight);
                    }
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
        updateAllStats(gameState, board, statsPanel, statsPanelRight, gameModeController);
    }

    /**
     * Updates all statistics panels with current game state.
     * Moved from StatsUpdater to bring UI logic closer to UI components.
     */
    private void updateAllStats(GameState state, Board board, StatsPanel statsPanel, RightStatsPanel statsPanelRight, GameModeController gameModeController) {
        if (state == null) return;

        // Get current score from board
        int currentScore = 0;
        if (board != null && board.getScore() != null && board.getScore().scoreProperty() != null) {
            currentScore = board.getScore().scoreProperty().getValue();

            state.setHighScore(currentScore, gameModeController);
        }

        if (statsPanel != null) {
            if (gameModeController != null) {
                String modeName = gameModeController.getCurrentMode().name();
                modeName = modeName.substring(0, 1) + modeName.substring(1).toLowerCase();
                statsPanel.updateMode(modeName);
            } else {
                statsPanel.updateMode("Endless");
            }

            // Get level from game mode controller if available
            int level = 1;
            if (gameModeController != null) {
                level = gameModeController.getCurrentLevel();
            }
            statsPanel.updateLevel(level);

            int highScore = state.getHighScore(gameModeController);
            statsPanel.updateHighScore(highScore);
        }

        if (statsPanelRight != null) {
            statsPanelRight.updateScore(currentScore);
            if (gameModeController != null) {
                int targetLines = gameModeController.getTargetLines();
                int currentLines = gameModeController.getLinesCleared();
                statsPanelRight.updateTarget(currentLines, targetLines);
            } else {
                statsPanelRight.updateTarget(0, 0);
            }
        }
    }

    /**
     * Updates the time display in the stats panel.
     * Moved from StatsUpdater to bring UI logic closer to UI components.
     */
    public void updateTime() {
        if (statsPanel == null || gameState == null) return;

        long totalSeconds = gameState.getElapsedSeconds();
        long minutes = totalSeconds / 60;
        long seconds = totalSeconds % 60;

        String timeString = String.format("%02d:%02d", minutes, seconds);
        statsPanel.updateTime(timeString);
    }

    // Expose for timer updates
    public StatsPanel getStatsPanel() {
        return statsPanel;
    }
}


