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

    // Null-check helper methods
    private void ifMenuController(Runnable action) {
        if (menuController != null) action.run();
    }

    private void ifGameRenderer(Runnable action) {
        if (gameRenderer != null) action.run();
    }

    private void ifPanelManager(Runnable action) {
        if (panelManager != null) action.run();
    }

    private void ifGameLifecycle(Runnable action) {
        if (gameLifecycle != null) action.run();
    }

    private void ifNotificationPanelManager(Runnable action) {
        if (notificationPanelManager != null) action.run();
    }

    private void ifEventListener(Runnable action) {
        if (eventListener != null) action.run();
    }

    private void ifBoard(Runnable action) {
        if (board != null) action.run();
    }

    private void ifGameStateManager(Runnable action) {
        if (gameStateManager != null) action.run();
    }

    private void ifAudioManager(Runnable action) {
        if (audioManager != null) action.run();
    }

    // MenuCallbacks implementation as inner class for better organization
    private class MenuCallbacksImpl implements MenuCallbacks {
        @Override
        public void onOpenSettings() {
            ifMenuController(() -> menuController.showSettingsMenu(gameBoard));
        }

        @Override
        public void onExitGame() {
            exitGame();
        }

        @Override
        public void onOpenKeyBindings() {
            ifMenuController(() -> {
                menuController.hideSettingsMenu();
                menuController.showKeyBindingsMenu(gameBoard);
            });
        }

        @Override
        public void onOpenThemes() {
            ifMenuController(() -> {
                menuController.hideSettingsMenu();
                menuController.showThemeMenu(gameBoard);
            });
        }

        @Override
        public void onBackFromSettings() {
            ifMenuController(() -> menuController.hideSettingsMenu());
        }

        @Override
        public void onBackFromKeyBindings() {
            ifMenuController(() -> {
                menuController.hideKeyBindingsMenu();
                menuController.showSettingsMenu(gameBoard);
            });
        }

        @Override
        public void onBindingsChanged() {
            // No action needed
        }

        @Override
        public void onBackFromTheme() {
            ifMenuController(() -> {
                menuController.hideThemeMenu();
                menuController.showSettingsMenu(gameBoard);
            });
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
            ifGameLifecycle(() -> {
                gameLifecycle.resumeTimers();
                isPause.setValue(Boolean.FALSE);
            });
            ifMenuController(() -> menuController.hidePauseMenu());
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
            ifMenuController(() -> menuController.showModeSelectionMenu(gameBoard));
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
            ifMenuController(() -> menuController.hideModeSelectionMenu());
        }
    }

    @Override
    public void initialize(URL location, java.util.ResourceBundle resources) {
        initializeConfigs();
        initializeUI();
        initializeMenus();
        initializeRenderingLayers();
        initializeInput();
    }

    private void initializeConfigs() {
        keyBindingsConfig = KeyBindingsConfig.getInstance();
        themeConfig = ThemeConfig.getInstance();
        gameModeConfig = new GameModeConfig();
        audioManager = AudioManager.getInstance();
        gameLifecycle = new GameLifecycle(audioManager);
        themeApplier = new ThemeApplier(themeConfig, audioManager);
    }

    private void initializeUI() {
        Font.loadFont(getClass().getClassLoader().getResource("digital.ttf").toExternalForm(), 38);
        BorderPane.setAlignment(gamePanel, Pos.CENTER);
        gamePanel.getStyleClass().add("game-panel");
        brickPanel.setVisible(false);

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
    }

    private void initializeMenus() {
        MenuFactory menuFactory = new MenuFactory(audioManager, new MenuCallbacksImpl());
        menuController = new MenuController(menuFactory, audioManager);
    }

    private void initializeRenderingLayers() {
        PlatformUtils.run(() -> {
            Scene scene = SceneAccessor.sceneOf(gameBoard);
            if (scene != null) {
                createRenderingLayers(scene);
                setupMenuAndPanels(scene);
                initializeGameStateManager();
            }
        });
    }

    private void createRenderingLayers(Scene scene) {
        Pane root = SceneAccessor.rootOf(gameBoard);
        if (root != null) {
            gameplayLayer = new Group();
            gameplayLayer.setViewOrder(1.0);
            gameplayLayer.setVisible(false);

            uiLayer = new Group();
            uiLayer.setViewOrder(0.0);

            if (root.getChildren().contains(gameBoard)) {
                root.getChildren().remove(gameBoard);
                gameplayLayer.getChildren().add(gameBoard);
            }
            if (root.getChildren().contains(brickPanel)) {
                root.getChildren().remove(brickPanel);
                gameplayLayer.getChildren().add(brickPanel);
            }

            ifNotificationPanelManager(() -> {
                notificationPanelManager.setGameBoard(gameBoard);
                notificationPanelManager.addToGameplayLayer(gameplayLayer, root);
            });

            root.getChildren().add(gameplayLayer);
            root.getChildren().add(uiLayer);
        }
    }

    private void setupMenuAndPanels(Scene scene) {
        ifMenuController(() -> {
            menuController.setMenuManager(MenuManager.ensure(null, gameBoard, uiLayer));
            menuController.showMainMenu(gameBoard);
        });

        panelManager = new PanelManager(gameBoard, board, gameState);
        if (gameModeController != null && panelManager != null) {
            panelManager.setGameModeController(gameModeController);
        }
        ifGameRenderer(() -> gameRenderer.setPanelManager(panelManager));
        centerGameBoard(scene);
        ifGameRenderer(() -> {
            gameRenderer.setBoardCentered(true);
            gameRenderer.refreshAfterCenter();
        });

        panelManager.initializeNextBrickPanel(scene);
        panelManager.initializeHoldBrickPanel(scene);
        panelManager.initializeStatsPanel(scene);
        panelManager.initializeStatsPanelRight(scene);

        ifNotificationPanelManager(() -> notificationPanelManager.positionGroups(scene));
    }

    private void initializeGameStateManager() {
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

    private void initializeInput() {
        gamePanel.setFocusTraversable(true);
        gamePanel.requestFocus();
        inputHandler = new InputHandler(
                gamePanel,
                gameBoard,
                keyBindingsConfig,
                menuController,
                gameLifecycle,
                isPause,
                isGameOver,
                createInputActions()
        );
        inputHandler.attach();
    }

    private InputHandler.InputActions createInputActions() {
        return new InputHandler.InputActions() {
            @Override
            public void moveLeft() {
                handleMove(EventType.LEFT, e -> e.onLeftEvent(new MoveEvent(EventType.LEFT, EventSource.USER)));
            }

            @Override
            public void moveRight() {
                handleMove(EventType.RIGHT, e -> e.onRightEvent(new MoveEvent(EventType.RIGHT, EventSource.USER)));
            }

            @Override
            public void rotate() {
                handleMove(EventType.ROTATE, e -> e.onRotateEvent(new MoveEvent(EventType.ROTATE, EventSource.USER)));
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
                ifBoard(() -> {
                    if (board.holdBrick()) {
                        ifGameRenderer(() -> gameRenderer.refreshBrick(board.getViewData()));
                        ifPanelManager(() -> {
                            panelManager.updateHoldBrickPanel();
                            panelManager.updateNextBrickPanel();
                        });
                    }
                });
            }
        };
    }

    private void handleMove(EventType eventType, java.util.function.Function<InputEventListener, ViewData> action) {
        ifEventListener(() -> {
            ifGameRenderer(() -> gameRenderer.postMoveRefresh(action.apply(eventListener)));
        });
    }

    public void initGameView(int[][] boardMatrix, ViewData brick) {
        ifGameRenderer(() -> gameRenderer.initGameView(boardMatrix, brick));

        if (board != null && gameModeConfig != null) {
            gameModeController = new GameModeController(gameModeConfig, board);
            ifPanelManager(() -> panelManager.setGameModeController(gameModeController));
            
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
                        // Stop garbage production when game over
                        ifGameLifecycle(() -> gameLifecycle.gameOver(isGameOver));
                        ifMenuController(() -> menuController.showGameOverMenu(gameBoard));
                    },
                    () -> {
                        ifGameLifecycle(() -> gameLifecycle.stopTimers());
                        ifAudioManager(() -> audioManager.playSoundEffect(GameConstants.SFX_WIN));
                        ifMenuController(() -> menuController.showWinMenu(gameBoard));
                    },
                    () -> {
                        ifGameRenderer(() -> {
                            ifBoard(() -> gameRenderer.refreshGameBackground(board.getBoardMatrix()));
                        });
                    }
            );
        }

        // Initialize timers in lifecycle (will be updated with mode speed)
        ifGameLifecycle(() -> {
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
                        ifPanelManager(() -> panelManager.updateTime());
                    },
                    initialSpeed
            );
        });

        isPause.setValue(Boolean.TRUE);
    }

    private void updateGameSpeed(int speedMs) {
        ifGameLifecycle(() -> gameLifecycle.updateSpeed(speedMs, () -> moveDown(new MoveEvent(EventType.DOWN, EventSource.THREAD))));
    }

    private Paint getFillColor(int i) {
        // Special handling for garbage blocks
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
            ifGameRenderer(() -> gameRenderer.postMoveRefresh(downData.getViewData()));
        }
        gamePanel.requestFocus();
    }

    private void hardDrop() {
        if (isPause.getValue() == Boolean.FALSE) {
            DownData downData = eventListener.onHardDropEvent();
            if (lineClearHandler != null) {
                lineClearHandler.handleLineClear(downData, true);
            }
            ifGameRenderer(() -> gameRenderer.postMoveRefresh(downData.getViewData()));
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
                ifGameRenderer(() -> gameRenderer.postMoveRefresh(downData.getViewData()));
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
                ifPanelManager(() -> panelManager.updateStatsPanels());
            });
        }
    }

    public void gameOver() {
        ifGameStateManager(() -> gameStateManager.handleGameOver());
    }

    public void newGame(ActionEvent actionEvent) {
        ifGameLifecycle(() -> gameLifecycle.stopTimers());
        eventListener.createNewGame();
        ifPanelManager(() -> {
            panelManager.updateNextBrickPanel();
            panelManager.updateHoldBrickPanel();
        });

        gameState.resetLines();
        gameState.startGame();
        ifPanelManager(() -> panelManager.updateStatsPanels());

        ifGameLifecycle(() -> {
            gameLifecycle.startTimers();
            isPause.setValue(Boolean.FALSE);
            isGameOver.setValue(Boolean.FALSE);
        });
        gamePanel.requestFocus();
    }

    public void pauseGame(ActionEvent actionEvent) {
        togglePauseMenu();
    }


    private void centerGameBoard(Scene scene) {
        LayoutHelper.centerGameBoard(scene, gameBoard);

        ifPanelManager(() -> panelManager.positionHoldBrickPanel(scene));
        ifPanelManager(() -> panelManager.positionNextBrickPanel(scene));
        ifPanelManager(() -> panelManager.positionStatsPanel(scene));
        ifPanelManager(() -> panelManager.positionStatsPanelRight(scene));

        ifNotificationPanelManager(() -> notificationPanelManager.positionGroups(scene));
    }

    private void togglePauseMenu() {
        ifGameStateManager(() -> gameStateManager.togglePause());
    }

    private void returnToMainMenu() {
        ifGameStateManager(() -> gameStateManager.returnToMainMenu());
    }

    private void restartGame() {
        ifNotificationPanelManager(() -> notificationPanelManager.reset());
        if (gameState != null) {
            gameState.startGame();
        }
        ifGameStateManager(() -> gameStateManager.restartGame());
        gamePanel.requestFocus();
    }

    public void showMainMenu() {
        ifMenuController(() -> menuController.showMainMenu(gameBoard));
    }

    public void hideMainMenu() {
        ifMenuController(() -> menuController.hideMainMenu());
    }

    private void startGame() {
        ifNotificationPanelManager(() -> notificationPanelManager.reset());

        if (gameState != null) {
            gameState.startGame();
        }

        if (board != null && gameModeConfig != null) {
            gameModeController = new GameModeController(gameModeConfig, board);
            ifPanelManager(() -> panelManager.setGameModeController(gameModeController));
            
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
                    () -> ifGameStateManager(() -> gameStateManager.handleGameOver()),
                    () -> {
                        ifGameLifecycle(() -> gameLifecycle.stopTimers());
                        ifAudioManager(() -> audioManager.playSoundEffect(GameConstants.SFX_WIN));
                        ifMenuController(() -> menuController.showWinMenu(gameBoard));
                    },
                    () -> {
                        ifGameRenderer(() -> {
                            ifBoard(() -> gameRenderer.refreshGameBackground(board.getBoardMatrix()));
                        });
                    }
            );
        }

        if (gameModeController != null && gameLifecycle != null) {
            int speed = gameModeController.getCurrentSpeedMs();
            gameLifecycle.updateSpeed(speed, () -> moveDown(new MoveEvent(EventType.DOWN, EventSource.THREAD)));
        }

        ifGameStateManager(() -> gameStateManager.startNewGame());
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
        ifMenuController(() -> menuController.showSettingsMenu(gameBoard));
    }

    public void hideSettingsMenu() {
        ifMenuController(() -> menuController.hideSettingsMenu());
    }

    public void showKeyBindingsMenu() {
        ifMenuController(() -> {
            menuController.showKeyBindingsMenu(gameBoard);
            KeyBindingsMenu km = menuController.getKeyBindingsMenu();
            if (km != null) km.refreshBindings();
        });
    }

    public void hideKeyBindingsMenu() {
        ifMenuController(() -> menuController.hideKeyBindingsMenu());
    }

    public void showThemeMenu() {
        ifMenuController(() -> menuController.showThemeMenu(gameBoard));
    }

    public void hideThemeMenu() {
        ifMenuController(() -> menuController.hideThemeMenu());
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
        ifBoard(() -> {
            ifGameRenderer(() -> {
                gameRenderer.refreshBrick(board.getViewData());
                gameRenderer.refreshGameBackground(board.getBoardMatrix());
            });
        });
        ifPanelManager(() -> panelManager.updateNextBrickPanel());
        ifPanelManager(() -> panelManager.updateHoldBrickPanel());
    }

    public void refreshGameBackground(int[][] boardMatrix) {
        ifGameRenderer(() -> gameRenderer.refreshGameBackground(boardMatrix));
    }

    public void setPrimaryStage(Stage stage) {
        this.primaryStage = stage;
    }

}