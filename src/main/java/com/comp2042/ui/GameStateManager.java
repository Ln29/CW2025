package com.comp2042.ui;

import com.comp2042.controller.AudioManager;
import com.comp2042.config.ThemeConfig;
import com.comp2042.controller.GameLifecycle;
import com.comp2042.controller.GameModeController;
import com.comp2042.controller.GameState;
import com.comp2042.core.Board;
import com.comp2042.input.InputEventListener;
import com.comp2042.ui.menu.MenuController;
import com.comp2042.ui.panels.PanelManager;
import com.comp2042.ui.util.PlatformUtils;
import javafx.beans.property.BooleanProperty;
import javafx.scene.Group;
import javafx.scene.layout.BorderPane;

/**
 * Manages game state transitions including pause, resume, restart, and game over.
 * Extracted from GuiController to improve separation of concerns.
 */
public class GameStateManager {
    
    private final GameLifecycle gameLifecycle;
    private final GameModeController gameModeController;
    private final MenuController menuController;
    private final AudioManager audioManager;
    private final ThemeConfig themeConfig;
    private final GameState gameState;
    private final Board board;
    private final GameRenderer gameRenderer;
    private final PanelManager panelManager;
    private final InputEventListener eventListener;
    private final BorderPane gameBoard;
    private final Group gameplayLayer;
    private final BooleanProperty isPause;
    private final BooleanProperty isGameOver;
    
    public GameStateManager(
            GameLifecycle gameLifecycle,
            GameModeController gameModeController,
            MenuController menuController,
            AudioManager audioManager,
            ThemeConfig themeConfig,
            GameState gameState,
            Board board,
            GameRenderer gameRenderer,
            PanelManager panelManager,
            InputEventListener eventListener,
            BorderPane gameBoard,
            Group gameplayLayer,
            BooleanProperty isPause,
            BooleanProperty isGameOver
    ) {
        this.gameLifecycle = gameLifecycle;
        this.gameModeController = gameModeController;
        this.menuController = menuController;
        this.audioManager = audioManager;
        this.themeConfig = themeConfig;
        this.gameState = gameState;
        this.board = board;
        this.gameRenderer = gameRenderer;
        this.panelManager = panelManager;
        this.eventListener = eventListener;
        this.gameBoard = gameBoard;
        this.gameplayLayer = gameplayLayer;
        this.isPause = isPause;
        this.isGameOver = isGameOver;
    }
    
    /**
     * Pauses the game and shows the pause menu.
     */
    public void pauseGame() {
        if (gameLifecycle == null || !gameLifecycle.hasTimers()) {
            return;
        }
        
        if (gameLifecycle != null) {
            gameLifecycle.pauseTimers();
            isPause.setValue(Boolean.TRUE);
        }
        if (menuController != null) {
            menuController.showPauseMenu(gameBoard);
        }
    }
    
    /**
     * Resumes the game from pause.
     */
    public void resumeGame() {
        if (gameLifecycle != null) {
            gameLifecycle.resumeTimers();
            isPause.setValue(Boolean.FALSE);
        }
        if (menuController != null) {
            menuController.hidePauseMenu();
        }
    }
    
    /**
     * Toggles between pause and resume states.
     */
    public void togglePause() {
        if (Boolean.TRUE.equals(isPause.getValue())) {
            resumeGame();
        } else {
            pauseGame();
        }
    }
    
    /**
     * Handles game over state.
     */
    public void handleGameOver() {
        if (gameLifecycle != null) {
            gameLifecycle.gameOver(isGameOver);
        }
        if (menuController != null) {
            menuController.showGameOverMenu(gameBoard);
        }
    }
    
    /**
     * Restarts the current game.
     */
    public void restartGame() {
        // Stop timers
        if (gameLifecycle != null) {
            gameLifecycle.stopTimers();
        }
        
        // Hide menus
        if (menuController != null) {
            menuController.hidePauseMenu();
            menuController.hideGameOverMenu();
        }
        
        // Reset game
        if (eventListener != null) {
            eventListener.createNewGame();
        }
        
        // Update panels
        if (panelManager != null) {
            panelManager.updateNextBrickPanel();
            panelManager.updateHoldBrickPanel();
        }
        
        // Reset game state
        if (gameState != null) {
            gameState.resetLines();
            gameState.setGameStartTimeNow();
        }
        if (panelManager != null) {
            panelManager.updateStatsPanels();
        }
        
        // Reset notifications
        // Note: NotificationService reset should be handled by caller if needed
        
        // Reset mode controller
        if (gameModeController != null) {
            gameModeController.reset();
        }
        
        // Play game music
        if (audioManager != null) {
            String musicFile = (themeConfig != null && themeConfig.getMusicFile() != null)
                    ? themeConfig.getMusicFile()
                    : "default.wav";
            audioManager.playGameMusic(musicFile);
        }
        
        // Start timers
        if (gameLifecycle != null) {
            gameLifecycle.startTimers();
            isPause.setValue(Boolean.FALSE);
            isGameOver.setValue(Boolean.FALSE);
        }
    }
    
    /**
     * Returns to the main menu from gameplay.
     */
    public void returnToMainMenu() {
        // Stop timers
        if (gameLifecycle != null) {
            gameLifecycle.stopTimers();
        }
        if (gameModeController != null) {
            gameModeController.reset();
        }
        
        // Hide gameplay layer
        if (gameplayLayer != null) {
            gameplayLayer.setVisible(false);
        }
        
        // Hide menus
        if (menuController != null) {
            menuController.hidePauseMenu();
            menuController.hideGameOverMenu();
            menuController.hideMainMenu();
            menuController.showMainMenu(gameBoard);
        }
        
        // Reset state
        isPause.setValue(Boolean.TRUE);
        isGameOver.setValue(Boolean.FALSE);
        
        // Play main menu music
        if (audioManager != null) {
            audioManager.playMainMenuMusic();
        }
    }
    
    /**
     * Starts a new game.
     */
    public void startNewGame() {
        // Hide main menu
        if (menuController != null) {
            menuController.hideMainMenu();
        }
        
        // Show gameplay layer
        if (gameplayLayer != null) {
            gameplayLayer.setVisible(true);
        }
        
        // Reset notifications
        // NotificationService reset should be handled by caller if needed
        
        // Create new game
        if (eventListener != null) {
            eventListener.createNewGame();
        }
        
        // Update renderer
        if (board != null && gameRenderer != null) {
            gameRenderer.refreshBrick(board.getViewData());
            gameRenderer.refreshGameBackground(board.getBoardMatrix());
        }
        
        // Update panels
        if (panelManager != null) {
            panelManager.updateNextBrickPanel();
            panelManager.updateHoldBrickPanel();
        }
        
        // Play game music
        if (audioManager != null) {
            String musicFile = (themeConfig != null && themeConfig.getMusicFile() != null)
                    ? themeConfig.getMusicFile()
                    : "default.wav";
            audioManager.playGameMusic(musicFile);
        }
        
        // Reset game state
        if (gameState != null) {
            gameState.resetLines();
            gameState.setGameStartTimeNow();
        }
        if (panelManager != null) {
            PlatformUtils.run(() -> panelManager.updateStatsPanels());
        }
        
        // Start timers
        if (gameLifecycle != null) {
            gameLifecycle.startTimers();
            isPause.setValue(Boolean.FALSE);
            isGameOver.setValue(Boolean.FALSE);
        }
    }
}

