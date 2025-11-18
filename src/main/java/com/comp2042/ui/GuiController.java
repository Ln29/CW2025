package com.comp2042.ui;

import com.comp2042.audio.AudioManager;
import com.comp2042.config.GameConstants;
import com.comp2042.config.GameModeConfig;
import com.comp2042.config.KeyBindingsConfig;
import com.comp2042.config.StatsUpdater;
import com.comp2042.controller.GameLifecycle;
import com.comp2042.controller.GameModeController;
import com.comp2042.controller.GameState;
import com.comp2042.core.GameMode;
import com.comp2042.core.Board;
import com.comp2042.core.DownData;
import com.comp2042.core.ViewData;
import com.comp2042.input.EventSource;
import com.comp2042.input.EventType;
import com.comp2042.input.InputEventListener;
import com.comp2042.input.InputHandler;
import com.comp2042.input.MoveEvent;
import com.comp2042.render.GameRenderer;
import com.comp2042.theme.ThemeApplier;
import com.comp2042.theme.ThemeConfig;
import com.comp2042.ui.menu.KeyBindingsMenu;
import com.comp2042.ui.menu.MenuCallbacks;
import com.comp2042.ui.menu.MenuController;
import com.comp2042.ui.menu.MenuFactory;
import com.comp2042.ui.menu.MenuManager;
import com.comp2042.ui.menu.ModeSelectionMenu;
import com.comp2042.ui.menu.ThemeMenu;
import com.comp2042.ui.notification.NotificationService;
import com.comp2042.ui.panels.PanelManager;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.IntegerProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.Group;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.GridPane;
import javafx.scene.paint.Color;
import javafx.scene.paint.Paint;
import javafx.scene.text.Font;
import javafx.stage.Stage;

import java.net.URL;
import java.util.ResourceBundle;

public class GuiController implements Initializable {

    private static final int BRICK_SIZE = GameConstants.BRICK_SIZE;
    private static final int HIDDEN_ROW_COUNT = GameConstants.HIDDEN_ROW_COUNT; // Top 2 rows are hidden

    @FXML
    private BorderPane gameBoard;

    @FXML
    private GridPane gamePanel;

    @FXML
    private Group comboNotificationGroup;

    @FXML
    private Group scoreNotificationGroup;

    @FXML
    private Group centerNotificationGroup;

    @FXML
    private GridPane brickPanel;

    private MenuController menuController;
    private KeyBindingsConfig keyBindingsConfig;
    private ThemeConfig themeConfig;
    private GameModeConfig gameModeConfig;
    private AudioManager audioManager;
    private GameLifecycle gameLifecycle;
    private GameModeController gameModeController;
    private PanelManager panelManager;
    private ThemeApplier themeApplier;
    private Stage primaryStage;
    private InputHandler inputHandler;
    private GameRenderer gameRenderer;
    private Board board;
    private NotificationService notificationService;

    // Game state
    private GameState gameState = new GameState();
    private StatsUpdater statsUpdater = new StatsUpdater();

    private InputEventListener eventListener;

    private final BooleanProperty isPause = new SimpleBooleanProperty();
    private final BooleanProperty isGameOver = new SimpleBooleanProperty();

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        Font.loadFont(getClass().getClassLoader().getResource("digital.ttf").toExternalForm(), 38);
        BorderPane.setAlignment(gamePanel, Pos.CENTER);
        gamePanel.setStyle("-fx-background-color: rgba(0, 0, 0, 0.7);");
        brickPanel.setVisible(false);

        keyBindingsConfig = KeyBindingsConfig.getInstance();
        themeConfig = ThemeConfig.getInstance();
        gameModeConfig = new GameModeConfig();
        audioManager = AudioManager.getInstance();
        gameLifecycle = new GameLifecycle(audioManager);
        themeApplier = new ThemeApplier(themeConfig, audioManager);
        applyTheme(themeConfig.getCurrentTheme(), false);
        notificationService = new NotificationService(audioManager, comboNotificationGroup, scoreNotificationGroup, centerNotificationGroup, GameConstants.NOTIFICATION_MAX);
        gameRenderer = new GameRenderer(
                gameBoard,
                gamePanel,
                brickPanel,
                BRICK_SIZE,
                HIDDEN_ROW_COUNT,
                this::getFillColor,
                isPause,
                () -> board,
                null
        );
        // Menus factory with callbacks
        MenuFactory menuFactory = new MenuFactory(audioManager, new MenuCallbacks() {
            @Override
            public void onOpenSettings() {
                if (menuController != null) menuController.showSettingsMenu(gameBoard);
            }
            @Override
            public void onExitGame() {
                exitGame();
            }
            @Override
            public void onOpenKeyBindings() {
                if (menuController != null) {
                    menuController.hideSettingsMenu();
                    menuController.showKeyBindingsMenu(gameBoard);
                }
            }
            @Override
            public void onOpenThemes() {
                if (menuController != null) {
                    menuController.hideSettingsMenu();
                    menuController.showThemeMenu(gameBoard);
                }
            }
            @Override
            public void onBackFromSettings() {
                if (menuController != null) menuController.hideSettingsMenu();
            }
            @Override
            public void onBackFromKeyBindings() {
                if (menuController != null) {
                    menuController.hideKeyBindingsMenu();
                    menuController.showSettingsMenu(gameBoard);
                }
            }
            @Override
            public void onBindingsChanged() {
            }

            @Override
            public void onBackFromTheme() {
                if (menuController != null) {
                    menuController.hideThemeMenu();
                    menuController.showSettingsMenu(gameBoard);
                }
                if (audioManager != null && (Boolean.TRUE.equals(isPause.getValue()) || Boolean.TRUE.equals(isGameOver.getValue()))) {
                    audioManager.playMainMenuMusic();
                }
            }
            @Override
            public void onThemeSelected(ThemeMenu.Theme theme) {
                applyTheme(theme);
            }
            @Override
            public void onResumeGame() {
                if (gameLifecycle != null) {
                    gameLifecycle.resumeTimers();
                    isPause.setValue(Boolean.FALSE);
                }
                if (gameModeController != null) {
                    gameModeController.resumeTimers();
                }
                if (menuController != null) menuController.hidePauseMenu();
            }
            @Override
            public void onRestartGame() {
                restartGame();
            }
            @Override
            public void onOpenMainMenuFromPause() {
                returnToMainMenu();
            }
            @Override
            public void onRestartFromGameOver() {
                restartGame();
            }
            @Override
            public void onOpenMainMenuFromGameOver() {
                returnToMainMenu();
            }
            @Override
            public void onOpenModeSelection() {
                if (menuController != null) {
                    menuController.showModeSelectionMenu(gameBoard);
                }
            }
            @Override
            public void onModeSelected() {
                if (menuController != null && menuController.getModeSelectionMenu() != null) {
                    ModeSelectionMenu menu = menuController.getModeSelectionMenu();
                    gameModeConfig.setCurrentMode(menu.getSelectedMode());
                    gameModeConfig.setDifficulty(menu.getDifficulty());
                    if (menu.getSelectedMode() == GameMode.MARATHON) {
                        gameModeConfig.setMarathonTargetLines(menu.getMarathonTargetLines());
                    } else if (menu.getSelectedMode() == GameMode.SURVIVAL) {
                            gameModeConfig.setSurvivalDifficulty(menu.getSurvivalDifficulty());
                    }
                    menuController.hideModeSelectionMenu();
                    startGame();
                }
            }
            @Override
            public void onBackFromModeSelection() {
                if (menuController != null) {
                    menuController.hideModeSelectionMenu();
                }
            }
        });
        menuController = new MenuController(menuFactory, audioManager);

        Ui.run(() -> {
            Scene scene = SceneAccessor.sceneOf(gameBoard);
            if (scene != null) {
                if (menuController != null) {
                    menuController.setMenuManager(MenuManager.ensure(null, gameBoard));
                }
                panelManager = new PanelManager(gameBoard, board, statsUpdater, gameState);
                if (gameModeController != null && panelManager != null) {
                    panelManager.setGameModeController(gameModeController);
                }
                if (gameRenderer != null) {
                    gameRenderer.setPanelManager(panelManager);
                }
                centerGameBoard(scene);
                if (gameRenderer != null) {
                    gameRenderer.setBoardCentered(true);
                    gameRenderer.refreshAfterCenter();
                }

                panelManager.initializeNextBrickPanel(scene);
                panelManager.initializeHoldBrickPanel(scene);
                panelManager.initializeStatsPanel(scene);
                panelManager.initializeStatsPanelRight(scene);

                positionNotificationGroups(scene);
            }
        });

        gamePanel.setFocusTraversable(true);
        gamePanel.requestFocus();
        // Build input handler
        InputHandler.InputActions actions = new InputHandler.InputActions() {
            @Override
            public void moveLeft() {
                if (eventListener != null) {
                    gameRenderer.postMoveRefresh(eventListener.onLeftEvent(new MoveEvent(EventType.LEFT, EventSource.USER)));
                }
            }
            @Override
            public void moveRight() {
                if (eventListener != null) {
                    gameRenderer.postMoveRefresh(eventListener.onRightEvent(new MoveEvent(EventType.RIGHT, EventSource.USER)));
                }
            }
            @Override
            public void rotate() {
                if (eventListener != null) {
                    gameRenderer.postMoveRefresh(eventListener.onRotateEvent(new MoveEvent(EventType.ROTATE, EventSource.USER)));
                }
            }
            @Override
            public void softDrop() {
                moveDown(new MoveEvent(EventType.DOWN, EventSource.USER));
            }
            @Override
            public void hardDrop() {
                GuiController.this.hardDrop();
            }
            @Override
            public void hold() {
                if (board != null && board.holdBrick()) {
                    gameRenderer.refreshBrick(board.getViewData());
                    if (panelManager != null) {
                        panelManager.updateHoldBrickPanel();
                        panelManager.updateNextBrickPanel();
                    }
                }
            }
        };
        inputHandler = new InputHandler(
                gamePanel,
                gameBoard,
                keyBindingsConfig,
                menuController,
                gameLifecycle,
                isPause,
                isGameOver,
                actions
        );
        inputHandler.attach();
    }

    public void initGameView(int[][] boardMatrix, ViewData brick) {
        if (gameRenderer != null) {
            gameRenderer.initGameView(boardMatrix, brick);
        }

        // Initialize game mode controller
        if (board != null && gameModeConfig != null) {
            gameModeController = new GameModeController(gameModeConfig, board);
            if (panelManager != null) {
                panelManager.setGameModeController(gameModeController);
            }
            gameModeController.initTimers(
                    this::updateGameSpeed,
                    () -> {
                        // Stop garbage production when game over
                        if (gameModeController != null) {
                            gameModeController.stopTimers();
                        }
                        if (gameLifecycle != null) {
                            gameLifecycle.gameOver(isGameOver);
                        }
                        if (menuController != null) {
                            menuController.showGameOverMenu(gameBoard);
                        }
                    },
                    () -> {
                        // Game won
                        if (gameLifecycle != null) {
                            gameLifecycle.stopTimers();
                        }
                        // Stop garbage production when game won
                        if (gameModeController != null) {
                            gameModeController.stopTimers();
                        }
                        // Play win sound
                        if (audioManager != null) {
                            audioManager.playSoundEffect(GameConstants.SFX_WIN);
                        }
                        if (menuController != null) {
                            menuController.showWinMenu(gameBoard);
                        }
                    },
                    () -> {
                        if (gameRenderer != null && board != null) {
                            gameRenderer.refreshGameBackground(board.getBoardMatrix());
                        }
                    }
            );
        }

        if (gameLifecycle != null) {
            int initialSpeed = gameModeController != null ? gameModeController.getCurrentSpeedMs() : GameConstants.GAME_TICK_MS;
            gameLifecycle.initTimers(
                    () -> moveDown(new MoveEvent(EventType.DOWN, EventSource.THREAD)),
                    this::checkLockDelay,
                    () -> {
                        if (isPause.getValue() == Boolean.FALSE && gameState != null) {
                            gameState.incrementElapsedSeconds();
                        }
                        if (gameModeController != null && gameModeController.getCurrentMode() == com.comp2042.core.GameMode.SURVIVAL) {
                            long elapsedSeconds = gameState.getElapsedSeconds();
                            if (gameModeController.shouldSpawnGarbage(elapsedSeconds)) {
                                // Spawn garbage rows
                                gameModeController.spawnGarbageRows();
                            }
                        }
                        if (panelManager != null) {
                            statsUpdater.updateTime(gameState, panelManager.getStatsPanel());
                        }
                    },
                    initialSpeed
            );
        }

        isPause.setValue(Boolean.TRUE);
    }

    private void updateGameSpeed(int speedMs) {
        if (gameLifecycle != null) {
            gameLifecycle.updateSpeed(speedMs, () -> moveDown(new MoveEvent(EventType.DOWN, EventSource.THREAD)));
        }
    }

    private Paint getFillColor(int i) {
        if (themeConfig != null) {
            return themeConfig.getBrickColor(i);
        }
        Paint returnPaint;
        switch (i) {
            case 0: returnPaint = Color.TRANSPARENT; break;
            case 1: returnPaint = Color.AQUA; break;
            case 2: returnPaint = Color.BLUEVIOLET; break;
            case 3: returnPaint = Color.DARKGREEN; break;
            case 4: returnPaint = Color.YELLOW; break;
            case 5: returnPaint = Color.RED; break;
            case 6: returnPaint = Color.BEIGE; break;
            case 7: returnPaint = Color.BURLYWOOD; break;
            case 8: returnPaint = Color.GRAY; break;
            default: returnPaint = Color.WHITE; break;
        }
        return returnPaint;
    }


    private void moveDown(MoveEvent event) {
        if (isPause.getValue() == Boolean.FALSE) {
            DownData downData = eventListener.onDownEvent(event);
            if (downData.getClearRow() != null) {
                int removed = downData.getClearRow().getLinesRemoved();
                int bonus = downData.getClearRow().getScoreBonus();
                if (removed > 0) {
                    gameState.addClearedLines(removed);
                    if (gameModeController != null) {
                        gameModeController.onLinesCleared(removed);
                    }
                    if (panelManager != null) panelManager.updateStatsPanels();
                    if (notificationService != null && gameModeController != null) {
                        notificationService.checkLevelUp(gameModeController.getCurrentLevel());
                    }
                    if (notificationService != null) {
                        notificationService.onLinesCleared(removed, bonus);

                        // Apply combo multiplier to score
                        if (board != null && board.getScore() != null) {
                            int comboCount = notificationService.getComboCount();
                            if (comboCount > 1) {
                                int additionalScore = bonus * (comboCount - 1);
                                board.getScore().add(additionalScore);
                            }
                        }
                    }
                } else {
                    if (notificationService != null) {
                        notificationService.onLinesCleared(removed, bonus);
                    }
                }
            }
            if (gameRenderer != null) gameRenderer.postMoveRefresh(downData.getViewData());
        }
        gamePanel.requestFocus();
    }

    private void hardDrop() {
        if (isPause.getValue() == Boolean.FALSE) {
            DownData downData = eventListener.onHardDropEvent();
            if (downData.getClearRow() != null) {
                int removed = downData.getClearRow().getLinesRemoved();
                int bonus = downData.getClearRow().getScoreBonus();
                if (removed > 0) {
                    gameState.addClearedLines(removed);
                    if (gameModeController != null) {
                        gameModeController.onLinesCleared(removed);
                    }
                    if (panelManager != null) panelManager.updateStatsPanels();
                    if (notificationService != null && gameModeController != null) {
                        notificationService.checkLevelUp(gameModeController.getCurrentLevel());
                    }

                    if (notificationService != null) {
                        notificationService.onLinesCleared(removed, bonus);

                        if (board != null && board.getScore() != null) {
                            int comboCount = notificationService.getComboCount();
                            if (comboCount > 1) {
                                int additionalScore = bonus * (comboCount - 1);
                                board.getScore().add(additionalScore);
                            }
                        }
                    }
                } else {
                    if (notificationService != null) {
                        notificationService.onLinesCleared(removed, bonus);
                    }
                }
            }
            if (gameRenderer != null) gameRenderer.postMoveRefresh(downData.getViewData());
        }
        gamePanel.requestFocus();
    }

    private void checkLockDelay() {
        if (isPause.getValue() == Boolean.FALSE && board != null && eventListener != null) {
            if (board.shouldLockPiece()) {
                DownData downData = eventListener.onDownEvent(new MoveEvent(EventType.DOWN, EventSource.THREAD));
                if (downData.getClearRow() != null) {
                    int removed = downData.getClearRow().getLinesRemoved();
                    int bonus = downData.getClearRow().getScoreBonus();
                    if (removed > 0) {
                        gameState.addClearedLines(removed);
                        if (gameModeController != null) {
                            gameModeController.onLinesCleared(removed);
                        }
                        if (panelManager != null) panelManager.updateStatsPanels();
                        if (notificationService != null && gameModeController != null) {
                            notificationService.checkLevelUp(gameModeController.getCurrentLevel());
                        }
                        // Call onLinesCleared first to increment combo count
                        if (notificationService != null) {
                            notificationService.onLinesCleared(removed, bonus);

                            if (board != null && board.getScore() != null) {
                                int comboCount = notificationService.getComboCount();
                                if (comboCount > 1) {
                                    // Additional score = baseBonus * (comboCount - 1)
                                    int additionalScore = bonus * (comboCount - 1);
                                    board.getScore().add(additionalScore);
                                }
                            }
                        }
                    } else {
                        // No lines cleared - reset combo
                        if (notificationService != null) {
                            notificationService.onLinesCleared(removed, bonus);

                        }
                    }
                }
                if (gameRenderer != null) gameRenderer.postMoveRefresh(downData.getViewData());
            }
        }
    }

    public void setEventListener(InputEventListener eventListener) {
        this.eventListener = eventListener;
    }

    public void setBoard(Board board) {
        this.board = board;
    }

    public void bindScore(IntegerProperty integerProperty) {
        if (integerProperty != null) {
            integerProperty.addListener((obs, oldVal, newVal) -> {
                if (panelManager != null) panelManager.updateStatsPanels();
            });
        }
    }

    public void gameOver() {
        if (gameLifecycle != null) {
            gameLifecycle.gameOver(isGameOver);
        }
        if (gameModeController != null) {
            gameModeController.stopTimers();
        }
        if (menuController != null) {
            menuController.showGameOverMenu(gameBoard);
        }
    }

    public void newGame(ActionEvent actionEvent) {
        if (gameLifecycle != null) {
            gameLifecycle.stopTimers();
        }
        eventListener.createNewGame();
        if (panelManager != null) panelManager.updateNextBrickPanel();

        gameState.resetLines();
        gameState.setGameStartTimeNow();
        if (panelManager != null) panelManager.updateStatsPanels();
        statsUpdater.startTimer(gameState);

        if (gameLifecycle != null) {
            gameLifecycle.startTimers();
            isPause.setValue(Boolean.FALSE);
            isGameOver.setValue(Boolean.FALSE);
        }
        gamePanel.requestFocus();
    }

    public void pauseGame(ActionEvent actionEvent) {
        togglePauseMenu();
    }


    private void centerGameBoard(Scene scene) {
        LayoutHelper.centerGameBoard(scene, gameBoard);

        if (panelManager != null) panelManager.positionHoldBrickPanel(scene);
        if (panelManager != null) panelManager.positionNextBrickPanel(scene);
        if (panelManager != null) panelManager.positionStatsPanel(scene);
        if (panelManager != null) panelManager.positionStatsPanelRight(scene);

        positionNotificationGroups(scene);
    }

    private void positionNotificationGroups(Scene scene) {
        if (scene == null || comboNotificationGroup == null || scoreNotificationGroup == null) return;

        // Get StatsPanelRight position to position notifications above it
        double boardWidth = GameConstants.BOARD_COLS * GameConstants.BRICK_SIZE + GameConstants.BOARD_BORDER_TOTAL_PX;
        double boardHeight = GameConstants.BOARD_VISIBLE_ROWS * GameConstants.BRICK_SIZE + GameConstants.BOARD_BORDER_TOTAL_PX;
        double boardX = gameBoard.getLayoutX();
        double boardY = gameBoard.getLayoutY();

        // Calculate StatsPanelRight position (same logic as in StatsPanelRight.position())
        double nextPanelX = boardX + boardWidth + 30;
        double nextPanelHeight = boardHeight / 2;
        double statsPanelRightY = boardY + nextPanelHeight + 200; // StatsPanelRight Y position

        // Position combo notifications above score notifications
        double comboX = nextPanelX;
        double comboY = statsPanelRightY - 250; // 250px above score notifications

        // Score notifications: 60px above StatsPanelRight
        double scoreX = nextPanelX;
        double scoreY = statsPanelRightY - 200;

        comboNotificationGroup.setLayoutX(comboX);
        comboNotificationGroup.setLayoutY(comboY);

        scoreNotificationGroup.setLayoutX(scoreX);
        scoreNotificationGroup.setLayoutY(scoreY);

        // Position center notification group in the middle of the game board
        if (centerNotificationGroup != null) {
            double centerX = boardX + boardWidth / 2;
            double centerY = boardY + boardHeight / 2;
            centerNotificationGroup.setLayoutX(centerX - 110);
            centerNotificationGroup.setLayoutY(centerY - 100);
        }
    }

    private void togglePauseMenu() {
        if (gameLifecycle == null || !gameLifecycle.hasTimers()) return;
        if (Boolean.TRUE.equals(isPause.getValue())) {
            if (gameLifecycle != null) {
                gameLifecycle.resumeTimers();
                isPause.setValue(Boolean.FALSE);
            }
            if (gameModeController != null) {
                gameModeController.resumeTimers();
            }
            if (menuController != null) menuController.hidePauseMenu();
        } else {
            if (gameLifecycle != null) {
                gameLifecycle.pauseTimers();
                isPause.setValue(Boolean.TRUE);
            }
            if (gameModeController != null) {
                gameModeController.pauseTimers();
            }
            if (menuController != null) menuController.showPauseMenu(gameBoard);
        }
    }

    private void returnToMainMenu() {
        if (gameLifecycle != null) {
            gameLifecycle.stopTimers();
        }
        if (gameModeController != null) {
            gameModeController.stopTimers();
            gameModeController.reset();
        }

        if (menuController != null) {
            menuController.hidePauseMenu();
            menuController.hideGameOverMenu();
            menuController.hideMainMenu();
            menuController.showMainMenu(gameBoard);
        }

        isPause.setValue(Boolean.TRUE);
        isGameOver.setValue(Boolean.FALSE);

        if (audioManager != null) {
            audioManager.playMainMenuMusic();
        }
    }

    private void restartGame() {
        if (gameLifecycle != null) {
            gameLifecycle.stopTimers();
        }
        if (gameModeController != null) {
            gameModeController.stopTimers();
        }

        if (menuController != null) {
            menuController.hidePauseMenu();
            menuController.hideGameOverMenu();
        }
        eventListener.createNewGame();
        if (panelManager != null) panelManager.updateNextBrickPanel();

        gameState.resetLines();
        gameState.setGameStartTimeNow();
        if (panelManager != null) panelManager.updateStatsPanels();
        statsUpdater.startTimer(gameState);

        if (notificationService != null) {
            notificationService.resetCombo();
            notificationService.resetLevelTracking();
        }

        if (gameModeController != null) {
            gameModeController.reset();
        }

        if (audioManager != null) {
            String musicFile = (themeConfig != null && themeConfig.getMusicFile() != null)
                    ? themeConfig.getMusicFile()
                    : "default.wav";
            audioManager.playGameMusic(musicFile);
        }

        if (gameLifecycle != null) {
            gameLifecycle.startTimers();
            isPause.setValue(Boolean.FALSE);
            isGameOver.setValue(Boolean.FALSE);
        }
        if (gameModeController != null) {
            gameModeController.startTimers();
        }
        gamePanel.requestFocus();
    }

    public void showMainMenu() {
        if (menuController != null) menuController.showMainMenu(gameBoard);
    }

    public void hideMainMenu() {
        if (menuController != null) menuController.hideMainMenu();
    }

    private void startGame() {
        hideMainMenu();

        if (notificationService != null) {
            notificationService.resetCombo();
            notificationService.resetLevelTracking();
        }

        if (eventListener != null) {
            eventListener.createNewGame();
        }

        if (board != null && gameRenderer != null) {
            gameRenderer.refreshBrick(board.getViewData());
            gameRenderer.refreshGameBackground(board.getBoardMatrix());
        }
        if (panelManager != null) {
            panelManager.updateNextBrickPanel();
            panelManager.updateHoldBrickPanel();
        }

        if (audioManager != null) {
            String musicFile = (themeConfig != null && themeConfig.getMusicFile() != null)
                    ? themeConfig.getMusicFile()
                    : "default.wav";
            audioManager.playGameMusic(musicFile);
        }
        gameState.resetLines();
        gameState.setGameStartTimeNow();
        if (panelManager != null) Ui.run(() -> panelManager.updateStatsPanels());

        statsUpdater.startTimer(gameState);

        // Reinitialize game mode controller with new config
        if (board != null && gameModeConfig != null) {
            gameModeController = new GameModeController(gameModeConfig, board);
            if (panelManager != null) {
                panelManager.setGameModeController(gameModeController);
            }
            gameModeController.initTimers(
                    this::updateGameSpeed,
                    () -> {
                        if (gameModeController != null) {
                            gameModeController.stopTimers();
                        }
                        if (gameLifecycle != null) {
                            gameLifecycle.gameOver(isGameOver);
                        }
                        if (menuController != null) {
                            menuController.showGameOverMenu(gameBoard);
                        }
                    },
                    () -> {
                        if (gameLifecycle != null) {
                            gameLifecycle.stopTimers();
                        }
                        if (gameModeController != null) {
                            gameModeController.stopTimers();
                        }
                        if (audioManager != null) {
                            audioManager.playSoundEffect(GameConstants.SFX_WIN);
                        }
                        if (menuController != null) {
                            menuController.showWinMenu(gameBoard);
                        }
                    },
                    () -> {
                        if (gameRenderer != null && board != null) {
                            gameRenderer.refreshGameBackground(board.getBoardMatrix());
                        }
                    }
            );
        }

        if (gameModeController != null && gameLifecycle != null) {
            int speed = gameModeController.getCurrentSpeedMs();
            gameLifecycle.updateSpeed(speed, () -> moveDown(new MoveEvent(EventType.DOWN, EventSource.THREAD)));
        }

        if (gameLifecycle != null) {
            gameLifecycle.startTimers();
            isPause.setValue(Boolean.FALSE);
            isGameOver.setValue(Boolean.FALSE);
        }
        if (gameModeController != null) {
            gameModeController.startTimers();
        }
        gamePanel.requestFocus();
    }

    private void exitGame() {
        if (primaryStage != null) {
            primaryStage.close();
        } else {
            Scene scene = gameBoard.getScene();
            if (scene != null) {
                Stage stage = (Stage) scene.getWindow();
                if (stage != null) {
                    stage.close();
                }
            }
        }
    }

    public void showSettingsMenu() {
        if (menuController != null) menuController.showSettingsMenu(gameBoard);
    }

    public void hideSettingsMenu() {
        if (menuController != null) menuController.hideSettingsMenu();
    }

    public void showKeyBindingsMenu() {
        if (menuController != null) menuController.showKeyBindingsMenu(gameBoard);
        KeyBindingsMenu km = menuController != null ? menuController.getKeyBindingsMenu() : null;
        if (km != null) km.refreshBindings();
    }

    public void hideKeyBindingsMenu() {
        if (menuController != null) menuController.hideKeyBindingsMenu();
    }

    public void showThemeMenu() {
        if (menuController != null) menuController.showThemeMenu(gameBoard);
    }

    public void hideThemeMenu() {
        if (menuController != null) menuController.hideThemeMenu();
    }


    // Single entry point for applying a theme consistently
    private void applyTheme(ThemeMenu.Theme theme) {
        applyTheme(theme, true);
    }

    private void applyTheme(ThemeMenu.Theme theme, boolean playMusic) {
        Scene scene = SceneAccessor.sceneOf(gameBoard);
        if (themeApplier != null) {
            themeApplier.apply(theme, scene, this::refreshAllBrickDisplays, playMusic);
        }
    }

    private void refreshAllBrickDisplays() {
        if (board != null) {
            if (gameRenderer != null) {
                gameRenderer.refreshBrick(board.getViewData());
                gameRenderer.refreshGameBackground(board.getBoardMatrix());
            }
        }
        if (panelManager != null) panelManager.updateNextBrickPanel();
        if (panelManager != null) panelManager.updateHoldBrickPanel();
    }

    public void refreshGameBackground(int[][] boardMatrix) {
        if (gameRenderer != null) {
            gameRenderer.refreshGameBackground(boardMatrix);
        }
    }

    public void setPrimaryStage(Stage stage) {
        this.primaryStage = stage;
    }

}