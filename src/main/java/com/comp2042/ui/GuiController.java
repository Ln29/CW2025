package com.comp2042.ui;

import com.comp2042.controller.AudioManager;
import com.comp2042.config.GameConstants;
import com.comp2042.config.GameModeConfig;
import com.comp2042.config.KeyBindingsConfig;
import com.comp2042.controller.GameLifecycle;
import com.comp2042.controller.GameModeController;
import com.comp2042.controller.GameState;
import com.comp2042.core.mode.GameMode;
import com.comp2042.core.Board;
import com.comp2042.core.DownData;
import com.comp2042.core.ViewData;
import com.comp2042.input.InputEventListener;
import com.comp2042.input.InputHandler;
import com.comp2042.input.MoveEvent;
import com.comp2042.input.EventType;
import com.comp2042.input.EventSource;
import com.comp2042.config.ThemeConfig;
import com.comp2042.ui.menu.KeyBindingsMenu;
import com.comp2042.ui.menu.MenuCallbacks;
import com.comp2042.ui.menu.MenuController;
import com.comp2042.ui.menu.MenuFactory;
import com.comp2042.ui.menu.MenuManager;
import com.comp2042.ui.menu.ModeSelectionMenu;
import com.comp2042.ui.menu.ThemeMenu;
import com.comp2042.ui.panels.NotificationPanelManager;
import com.comp2042.ui.panels.PanelManager;
import com.comp2042.ui.util.LayoutHelper;
import com.comp2042.ui.util.PlatformUtils;
import com.comp2042.ui.util.SceneAccessor;
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
import javafx.scene.layout.Pane;
import javafx.scene.paint.Color;
import javafx.scene.paint.Paint;
import javafx.scene.text.Font;
import javafx.stage.Stage;

import java.net.URL;

public class GuiController implements Initializable {

    private static final int BRICK_SIZE = GameConstants.BRICK_SIZE;
    private static final int HIDDEN_ROW_COUNT = GameConstants.HIDDEN_ROW_COUNT;

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

    // Rendering layers
    private Group gameplayLayer;
    private Group uiLayer;

    private MenuController menuController;
    private KeyBindingsConfig keyBindingsConfig;
    private ThemeConfig themeConfig;
    private GameModeConfig gameModeConfig;
    private AudioManager audioManager;
    private GameLifecycle gameLifecycle;
    private GameModeController gameModeController;
    private PanelManager panelManager;
    private NotificationPanelManager notificationPanelManager;
    private ThemeApplier themeApplier;

    private Stage primaryStage;

    private InputHandler inputHandler;
    private GameRenderer gameRenderer;
    private Board board;

    // Game state
    private GameState gameState = new GameState();

    private InputEventListener eventListener;
    
    private LineClearHandler lineClearHandler;
    private GameStateManager gameStateManager;

    private final BooleanProperty isPause = new SimpleBooleanProperty();
    private final BooleanProperty isGameOver = new SimpleBooleanProperty();

    @Override
    public void initialize(URL location, java.util.ResourceBundle resources) {
        Font.loadFont(getClass().getClassLoader().getResource("digital.ttf").toExternalForm(), 38);
        BorderPane.setAlignment(gamePanel, Pos.CENTER);
        gamePanel.getStyleClass().add("game-panel");
        brickPanel.setVisible(false);

        keyBindingsConfig = KeyBindingsConfig.getInstance();
        themeConfig = ThemeConfig.getInstance();
        gameModeConfig = new GameModeConfig();
        audioManager = AudioManager.getInstance();
        gameLifecycle = new GameLifecycle(audioManager);
        themeApplier = new ThemeApplier(themeConfig, audioManager);

        applyTheme(themeConfig.getCurrentTheme(), false);
        notificationPanelManager = new NotificationPanelManager(audioManager, comboNotificationGroup, scoreNotificationGroup, centerNotificationGroup);

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
                    // Update config from menu selections
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


        PlatformUtils.run(() -> {
            Scene scene = SceneAccessor.sceneOf(gameBoard);
            if (scene != null) {
                // Create rendering layers
                Pane root = SceneAccessor.rootOf(gameBoard);
                if (root != null) {
                    // Create gameplay layer (for board, bricks, ghost)
                    gameplayLayer = new Group();
                    gameplayLayer.setViewOrder(1.0); // Behind UI layer
                    gameplayLayer.setVisible(false); // Hide gameplay initially - show main menu first

                    // Create UI layer (for menus)
                    uiLayer = new Group();
                    uiLayer.setViewOrder(0.0); // Always on top

                    if (root.getChildren().contains(gameBoard)) {
                        root.getChildren().remove(gameBoard);
                        gameplayLayer.getChildren().add(gameBoard);
                    }
                    if (root.getChildren().contains(brickPanel)) {
                        root.getChildren().remove(brickPanel);
                        gameplayLayer.getChildren().add(brickPanel);
                    }
                    
                    if (notificationPanelManager != null) {
                        notificationPanelManager.setGameBoard(gameBoard);
                        notificationPanelManager.addToGameplayLayer(gameplayLayer, root);
                    }

                    root.getChildren().add(gameplayLayer);
                    root.getChildren().add(uiLayer);
                }


                if (menuController != null) {
                    menuController.setMenuManager(MenuManager.ensure(null, gameBoard, uiLayer));
                    menuController.showMainMenu(gameBoard);
                }
                panelManager = new PanelManager(gameBoard, board, gameState);
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

                if (notificationPanelManager != null) {
                    notificationPanelManager.positionGroups(scene);
                }
                
                // Initialize game state manager
                gameStateManager = new GameStateManager(
                        gameLifecycle,
                        gameModeController,
                        menuController,
                        audioManager,
                        themeConfig,
                        gameState,
                        board,
                        gameRenderer,
                        panelManager,
                        eventListener,
                        gameBoard,
                        gameplayLayer,
                        isPause,
                        isGameOver
                );
            }
        });

        gamePanel.setFocusTraversable(true);
        gamePanel.requestFocus();
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

        if (board != null && gameModeConfig != null) {
            gameModeController = new GameModeController(gameModeConfig, board);
            if (panelManager != null) {
                panelManager.setGameModeController(gameModeController);
            }
            
            // Initialize line clear handler
            if (notificationPanelManager != null && panelManager != null) {
                lineClearHandler = new LineClearHandler(
                        gameState,
                        gameModeController,
                        notificationPanelManager.getNotificationService(),
                        board,
                        panelManager
                );
            }
            
            gameModeController.initTimers(
                    this::updateGameSpeed,
                    () -> {
                        // Stop garbage production when game over (no separate timers in GameModeController)
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

        // Initialize timers in lifecycle (will be updated with mode speed)
        if (gameLifecycle != null) {
            int initialSpeed = gameModeController != null ? gameModeController.getCurrentSpeedMs() : GameConstants.GAME_TICK_MS;
            gameLifecycle.initTimers(
                    () -> moveDown(new MoveEvent(EventType.DOWN, EventSource.THREAD)),
                    this::checkLockDelay,
                    () -> {
                        if (isPause.getValue() == Boolean.FALSE && gameState != null) {
                            gameState.incrementElapsedSeconds();

                            if (gameModeController != null && gameModeController.getCurrentMode() == GameMode.SURVIVAL) {
                                long elapsedSeconds = gameState.getElapsedSeconds();
                                if (gameModeController.shouldSpawnGarbage(elapsedSeconds)) {
                                    gameModeController.spawnGarbageRows();
                                }
                            }
                        }
                        if (panelManager != null) {
                            panelManager.updateTime();
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
        // Special handling for garbage blocks (colorCode 8)
        if (i == 8) {
            return Color.GRAY;
        }
        return themeConfig.getBrickColor(i);
    }


    private void moveDown(MoveEvent event) {
        if (isPause.getValue() == Boolean.FALSE) {
            DownData downData = eventListener.onDownEvent(event);
            if (lineClearHandler != null) {
                lineClearHandler.handleLineClear(downData, false);
            }
            if (gameRenderer != null) gameRenderer.postMoveRefresh(downData.getViewData());
        }
        gamePanel.requestFocus();
    }

    private void hardDrop() {
        if (isPause.getValue() == Boolean.FALSE) {
            DownData downData = eventListener.onHardDropEvent();
            if (lineClearHandler != null) {
                lineClearHandler.handleLineClear(downData, true);
            }
            if (gameRenderer != null) gameRenderer.postMoveRefresh(downData.getViewData());
        }
        gamePanel.requestFocus();
    }

    private void checkLockDelay() {
        if (isPause.getValue() == Boolean.FALSE && board != null && eventListener != null) {
            if (board.shouldLockPiece()) {
                DownData downData = eventListener.onDownEvent(new MoveEvent(EventType.DOWN, EventSource.THREAD));
                if (lineClearHandler != null) {
                    lineClearHandler.handleLineClear(downData, false);
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
        if (gameStateManager != null) {
            gameStateManager.handleGameOver();
        }
    }

    public void newGame(ActionEvent actionEvent) {
        if (gameLifecycle != null) {
            gameLifecycle.stopTimers();
        }
        eventListener.createNewGame();
        if (panelManager != null) {
            panelManager.updateNextBrickPanel();
            panelManager.updateHoldBrickPanel();
        }

        gameState.resetLines();
        gameState.startGame();
        if (panelManager != null) panelManager.updateStatsPanels();

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

        if (notificationPanelManager != null) {
            notificationPanelManager.positionGroups(scene);
        }
    }

    private void togglePauseMenu() {
        if (gameStateManager != null) {
            gameStateManager.togglePause();
        }
    }

    private void returnToMainMenu() {
        if (gameStateManager != null) {
            gameStateManager.returnToMainMenu();
        }
    }

    private void restartGame() {
        if (notificationPanelManager != null) {
            notificationPanelManager.reset();
        }
        if (gameState != null) {
            gameState.startGame();
        }
        if (gameStateManager != null) {
            gameStateManager.restartGame();
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
        if (notificationPanelManager != null) {
            notificationPanelManager.reset();
        }

        if (gameState != null) {
            gameState.startGame();
        }

        if (board != null && gameModeConfig != null) {
            gameModeController = new GameModeController(gameModeConfig, board);
            if (panelManager != null) {
                panelManager.setGameModeController(gameModeController);
            }
            
            // Reinitialize line clear handler with new game mode controller
            if (notificationPanelManager != null && panelManager != null) {
                lineClearHandler = new LineClearHandler(
                        gameState,
                        gameModeController,
                        notificationPanelManager.getNotificationService(),
                        board,
                        panelManager
                );
            }
            
            // Reinitialize game state manager with new game mode controller
            gameStateManager = new GameStateManager(
                    gameLifecycle,
                    gameModeController,
                    menuController,
                    audioManager,
                    themeConfig,
                    gameState,
                    board,
                    gameRenderer,
                    panelManager,
                    eventListener,
                    gameBoard,
                    gameplayLayer,
                    isPause,
                    isGameOver
            );
            
            gameModeController.initTimers(
                    this::updateGameSpeed,
                    () -> {
                        if (gameStateManager != null) {
                            gameStateManager.handleGameOver();
                        }
                    },
                    () -> {
                        if (gameLifecycle != null) {
                            gameLifecycle.stopTimers();
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

        if (gameStateManager != null) {
            gameStateManager.startNewGame();
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