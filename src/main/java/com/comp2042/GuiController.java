package com.comp2042;

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
    private Group groupNotification;

    @FXML
    private GridPane brickPanel;

    private MenuController menuController;
    private KeyBindingsConfig keyBindingsConfig;
    private ThemeConfig themeConfig;
    private AudioManager audioManager;
    private GameLifecycle gameLifecycle;
    private PanelManager panelManager;
    private ThemeApplier themeApplier;
    // menus managed by MenuController
    private Stage primaryStage;
    // stats/side panels managed by PanelManager
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
        gamePanel.setStyle("-fx-background-color: rgba(0, 0, 0, 0.3);");
        brickPanel.setVisible(false); // Prevent flashing at (0,0)

        keyBindingsConfig = KeyBindingsConfig.getInstance();
        themeConfig = ThemeConfig.getInstance();
        audioManager = AudioManager.getInstance();
        gameLifecycle = new GameLifecycle(audioManager);
        themeApplier = new ThemeApplier(themeConfig, audioManager);
        // Apply current theme visuals once at startup (no game music yet)
        applyTheme(themeConfig.getCurrentTheme(), false);
        notificationService = new NotificationService(audioManager, groupNotification, GameConstants.NOTIFICATION_MAX);
        // Renderer (panelManager attached when ready in Ui.run)
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
            public void onStartGame() {
                startGame();
            }
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
                // no-op for now
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
                restartGame();
                if (menuController != null) {
                    menuController.hideMainMenu();
                    menuController.showMainMenu(gameBoard);
                }
            }
            @Override
            public void onRestartFromGameOver() {
                restartGame();
            }
            @Override
            public void onOpenMainMenuFromGameOver() {
                restartGame();
                if (menuController != null) {
                    menuController.hideMainMenu();
                    menuController.showMainMenu(gameBoard);
                }
            }
        });
        menuController = new MenuController(menuFactory, audioManager);

        // Center game board after scene is ready
        Ui.run(() -> {
            Scene scene = SceneAccessor.sceneOf(gameBoard);
            if (scene != null) {
                // Initialize menu/panel managers
                if (menuController != null) {
                    menuController.setMenuManager(MenuManager.ensure(null, gameBoard));
                }
                panelManager = new PanelManager(gameBoard, board, statsUpdater, gameState);
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

        // Initialize timers in lifecycle
        if (gameLifecycle != null) {
            gameLifecycle.initTimers(
                    () -> moveDown(new MoveEvent(EventType.DOWN, EventSource.THREAD)),
                    this::checkLockDelay,
                    () -> {
                        if (panelManager != null) {
                            statsUpdater.updateTime(gameState, panelManager.getStatsPanel());
                        }
                    }
            );
        }

        // Start paused (wait for main menu)
        isPause.setValue(Boolean.TRUE);
    }

    private Paint getFillColor(int i) {
        if (themeConfig != null) {
            return themeConfig.getBrickColor(i);
        }
        // Default colors
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
            default: returnPaint = Color.WHITE; break;
        }
        return returnPaint;
    }

    // Ghost stroke color computed in GhostRenderer (via GameRenderer)

    private void moveDown(MoveEvent event) {
        if (isPause.getValue() == Boolean.FALSE) {
            DownData downData = eventListener.onDownEvent(event);
            if (downData.getClearRow() != null) {
                int removed = downData.getClearRow().getLinesRemoved();
                int bonus = downData.getClearRow().getScoreBonus();
                if (removed > 0) {
                    gameState.addClearedLines(removed);
                    if (panelManager != null) panelManager.updateStatsPanels();
                }
                if (notificationService != null) {
                    notificationService.onLinesCleared(removed, bonus);
                }
            }
            if (gameRenderer != null) gameRenderer.postMoveRefresh(downData.getViewData());
            if (board != null) {
                board.resetHoldUsage();
            }
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
                    if (panelManager != null) panelManager.updateStatsPanels();
                }
                if (notificationService != null) {
                    notificationService.onLinesCleared(removed, bonus);
                }
            }
            if (gameRenderer != null) gameRenderer.postMoveRefresh(downData.getViewData());
            if (board != null) {
                board.resetHoldUsage();
            }
        }
        gamePanel.requestFocus();
    }

    private void checkLockDelay() {
        if (isPause.getValue() == Boolean.FALSE && board != null && eventListener != null) {
            if (board.shouldLockPiece()) {
                DownData downData = eventListener.onDownEvent(new MoveEvent(EventType.DOWN, EventSource.THREAD));
                if (downData.getClearRow() != null && downData.getClearRow().getLinesRemoved() > 0) {
                    gameState.addClearedLines(downData.getClearRow().getLinesRemoved());
                    if (panelManager != null) panelManager.updateStatsPanels();
                    if (notificationService != null) {
                        notificationService.onLinesCleared(downData.getClearRow().getLinesRemoved(), downData.getClearRow().getScoreBonus());
                    }
                }
                if (gameRenderer != null) gameRenderer.postMoveRefresh(downData.getViewData());
                if (board != null) {
                    board.resetHoldUsage();
                }
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

    // Grid lines are drawn by GridRenderer.drawGridLines

    private void centerGameBoard(Scene scene) {
        LayoutHelper.centerGameBoard(scene, gameBoard);

        if (panelManager != null) panelManager.positionHoldBrickPanel(scene);
        if (panelManager != null) panelManager.positionNextBrickPanel(scene);
        if (panelManager != null) panelManager.positionStatsPanel(scene);
        if (panelManager != null) panelManager.positionStatsPanelRight(scene);
        // Menus are centered when shown via MenuController/MenuManager
    }

    private void togglePauseMenu() {
        if (gameLifecycle == null || !gameLifecycle.hasTimers()) return;
        if (Boolean.TRUE.equals(isPause.getValue())) {
            if (gameLifecycle != null) {
                gameLifecycle.resumeTimers();
                isPause.setValue(Boolean.FALSE);
            }
            if (menuController != null) menuController.hidePauseMenu();
        } else {
            if (gameLifecycle != null) {
                gameLifecycle.pauseTimers();
                isPause.setValue(Boolean.TRUE);
            }
            if (menuController != null) menuController.showPauseMenu(gameBoard);
        }
    }

    private void restartGame() {
        if (gameLifecycle != null) {
            gameLifecycle.stopTimers();
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

        if (audioManager != null) {
            String musicFile = (themeConfig != null && themeConfig.getMusicFile() != null)
                    ? themeConfig.getMusicFile()
                    : "default.wav";
            audioManager.playGameMusic(musicFile);
        }

        // Reset state and update stats BEFORE timers start to avoid visible delay
        gameState.resetLines();
        gameState.setGameStartTimeNow();
        if (panelManager != null) Ui.run(() -> panelManager.updateStatsPanels());

        statsUpdater.startTimer(gameState);

        if (gameLifecycle != null) {
            gameLifecycle.startTimers();
            isPause.setValue(Boolean.FALSE);
            isGameOver.setValue(Boolean.FALSE);
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

    // Theme background/music/brick colors are applied via applyTheme(...)

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

    // Backwards-compatible delegation for existing callers
    public void refreshGameBackground(int[][] boardMatrix) {
        if (gameRenderer != null) {
            gameRenderer.refreshGameBackground(boardMatrix);
        }
    }

    public void setPrimaryStage(Stage stage) {
        this.primaryStage = stage;
    }

}