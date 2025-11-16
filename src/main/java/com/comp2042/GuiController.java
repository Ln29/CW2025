package com.comp2042;

import javafx.beans.property.BooleanProperty;
import javafx.beans.property.IntegerProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.Group;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Pane;
import javafx.scene.paint.Color;
import javafx.scene.paint.Paint;
import javafx.scene.shape.Rectangle;
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
    private PanelPositioner panelPositioner;
    private GridRenderer gridRenderer = new GridRenderer();
    private ThemeApplier themeApplier;
    // menus managed by MenuController
    private NextBrickPanel nextBrickPanel;
    private HoldBrickPanel holdBrickPanel;
    private Stage primaryStage;
    private StatsPanel statsPanel;
    private StatsPanelRight statsPanelRight;
    private GhostRenderer ghostRenderer;
    private Board board;
    private NotificationService notificationService;

    // Game state
    private GameState gameState = new GameState();
    private StatsUpdater statsUpdater = new StatsUpdater();

    private boolean boardCentered = false;
    private ViewData initialBrickData = null;

    private Rectangle[][] displayMatrix;
    private InputEventListener eventListener;
    private Rectangle[][] rectangles;

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
                // Initialize menu manager for menus
                if (menuController != null) {
                    menuController.setMenuManager(MenuManager.ensure(null, gameBoard));
                }
                panelPositioner = new PanelPositioner(gameBoard);
                centerGameBoard(scene);
                boardCentered = true;
                if (initialBrickData != null && rectangles != null && rectangles.length > 0) {
                    refreshBrick(initialBrickData);
                }

                if (nextBrickPanel == null){
                    initializeNextBrickPanel();
                }else{
                    updateNextBrickPanel();
                }
                if (holdBrickPanel == null) {
                    initializeHoldBrickPanel();
                } else {
                    updateHoldBrickPanel();
                }
                if (statsPanel == null) {
                    initializeStatsPanel();
                } else {
                    updateStatsPanel();
                }
                if (statsPanelRight == null) {
                    initializeStatsPanelRight();
                } else {
                    updateStatsPanelRight();
                }
            }
        });

        gamePanel.setFocusTraversable(true);
        gamePanel.requestFocus();
        // Build input action map
        java.util.Map<KeyBindingsConfig.Action, Runnable> actionHandlers = new java.util.EnumMap<>(KeyBindingsConfig.Action.class);
        actionHandlers.put(KeyBindingsConfig.Action.MOVE_LEFT, () -> {
            postMoveRefresh(eventListener.onLeftEvent(new MoveEvent(EventType.LEFT, EventSource.USER)));
        });
        actionHandlers.put(KeyBindingsConfig.Action.MOVE_RIGHT, () -> {
            postMoveRefresh(eventListener.onRightEvent(new MoveEvent(EventType.RIGHT, EventSource.USER)));
        });
        actionHandlers.put(KeyBindingsConfig.Action.ROTATE, () -> {
            postMoveRefresh(eventListener.onRotateEvent(new MoveEvent(EventType.ROTATE, EventSource.USER)));
        });
        actionHandlers.put(KeyBindingsConfig.Action.SOFT_DROP, () -> {
            moveDown(new MoveEvent(EventType.DOWN, EventSource.USER));
        });
        actionHandlers.put(KeyBindingsConfig.Action.HARD_DROP, this::hardDrop);
        actionHandlers.put(KeyBindingsConfig.Action.HOLD, () -> {
            if (board != null && board.holdBrick()) {
                refreshBrick(board.getViewData());
                updateHoldBrickPanel();
                updateNextBrickPanel();
            }
        });

        gamePanel.setOnKeyPressed(new EventHandler<KeyEvent>() {
            @Override
            public void handle(KeyEvent keyEvent) {
                KeyCode code = keyEvent.getCode();

                // Handle pause key via InputRouter
                boolean kbVisible = menuController != null && menuController.isKeyBindingsMenuVisible();
                if (InputRouter.shouldTogglePause(keyEvent, keyBindingsConfig, Boolean.TRUE.equals(isGameOver.getValue()), kbVisible)) {
                    if (gameLifecycle == null || !gameLifecycle.hasTimers()) {
                        return;
                    }
                    if (Boolean.TRUE.equals(isPause.getValue())) {
                        if (gameLifecycle != null) {
                            gameLifecycle.resumeTimers();
                            isPause.setValue(Boolean.FALSE);
                        }
                        if (menuController != null) {
                            menuController.hidePauseMenu();
                        }
                    } else {
                        if (gameLifecycle != null) {
                            gameLifecycle.pauseTimers();
                            isPause.setValue(Boolean.TRUE);
                        }
                        if (menuController != null) {
                            menuController.showPauseMenu(gameBoard);
                        }
                    }
                    return;
                }

                // Route events to active overlay
                if (menuController != null && menuController.routeKey(keyEvent)) {
                    return;
                }

                // Handle game controls via action map
                if (Boolean.FALSE.equals(isPause.getValue()) && Boolean.FALSE.equals(isGameOver.getValue()) && eventListener != null) {
                    KeyBindingsConfig.Action action = keyBindingsConfig.getAction(code);
                    java.util.Optional.ofNullable(actionHandlers.get(action)).ifPresent(r -> {
                        r.run();
                        keyEvent.consume();
                    });
                }
            }
        });
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
                rectangle.toFront();
            }
        }

        // Initialize brick rectangles
        rectangles = new Rectangle[brick.getBrickData().length][brick.getBrickData()[0].length];
        for (int i = 0; i < brick.getBrickData().length; i++) {
            for (int j = 0; j < brick.getBrickData()[i].length; j++) {
                Rectangle rectangle = new Rectangle(BRICK_SIZE, BRICK_SIZE);
                rectangle.setFill(getFillColor(brick.getBrickData()[i][j]));
                rectangles[i][j] = rectangle;
                brickPanel.add(rectangle, j, i);
            }
        }
        initialBrickData = brick;

        // Initialize ghost renderer
        ghostRenderer = new GhostRenderer(gameBoard, gamePanel, brickPanel, BRICK_SIZE, HIDDEN_ROW_COUNT, this::getFillColor);
        Ui.run(() -> {
            Scene scene = gameBoard.getScene();
            if (scene != null) {
                ghostRenderer.addToScene(scene);
            }
        });
        gridRenderer.drawGridLines(gamePanel, BRICK_SIZE);

        // Initialize timers in lifecycle
        if (gameLifecycle != null) {
            gameLifecycle.initTimers(
                    () -> moveDown(new MoveEvent(EventType.DOWN, EventSource.THREAD)),
                    this::checkLockDelay,
                    () -> statsUpdater.updateTime(gameState, statsPanel)
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

    // Ghost stroke color computed in GhostRenderer

    private void refreshBrick(ViewData brick) {
        if (isPause.getValue() == Boolean.FALSE) {
            // Position brick panel only when board is centered
            if (boardCentered && gameBoard.getLayoutX() > 0) {
                positionBrickPanel(brick);
                brickPanel.setVisible(true);
                brickPanel.toFront();
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

            if (ghostRenderer != null && board != null) {
                ghostRenderer.render(board.getGhostBrick(), boardCentered);
            }
        }
    }

    private void postMoveRefresh(ViewData viewData) {
        refreshBrick(viewData);
        updateNextBrickPanel();
        statsUpdater.updateAllStats(gameState, board, statsPanel, statsPanelRight);
    }

    public void refreshGameBackground(int[][] board) {
        if (gridRenderer != null) {
            gridRenderer.refreshGameBackground(board, displayMatrix, this::getFillColor, HIDDEN_ROW_COUNT);
        }
    }

    private void setRectangleData(int color, Rectangle rectangle) {
        if (gridRenderer != null) {
            gridRenderer.setRectangleData(color, rectangle, this::getFillColor);
        }
    }

    private void moveDown(MoveEvent event) {
        if (isPause.getValue() == Boolean.FALSE) {
            DownData downData = eventListener.onDownEvent(event);
            if (downData.getClearRow() != null) {
                int removed = downData.getClearRow().getLinesRemoved();
                int bonus = downData.getClearRow().getScoreBonus();
                if (removed > 0) {
                    gameState.addClearedLines(removed);
                    statsUpdater.updateAllStats(gameState, board, statsPanel, statsPanelRight);
                }
                if (notificationService != null) {
                    notificationService.onLinesCleared(removed, bonus);
                }
            }
            postMoveRefresh(downData.getViewData());
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
                    statsUpdater.updateAllStats(gameState, board, statsPanel, statsPanelRight);
                }
                if (notificationService != null) {
                    notificationService.onLinesCleared(removed, bonus);
                }
            }
            postMoveRefresh(downData.getViewData());
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
                    statsUpdater.updateAllStats(gameState, board, statsPanel, statsPanelRight);
                    if (notificationService != null) {
                        notificationService.onLinesCleared(downData.getClearRow().getLinesRemoved(), downData.getClearRow().getScoreBonus());
                    }
                }
                postMoveRefresh(downData.getViewData());
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
                statsUpdater.updateAllStats(gameState, board, statsPanel, statsPanelRight);
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
        updateNextBrickPanel();

        gameState.resetLines();
        gameState.setGameStartTimeNow();
        statsUpdater.updateAllStats(gameState, board, statsPanel, statsPanelRight);
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

    private void initializeNextBrickPanel() {
        nextBrickPanel = new NextBrickPanel();
        Ui.run(() -> {
            Scene scene = SceneAccessor.sceneOf(gameBoard);
            if (scene != null) {
                Pane rootPane = SceneAccessor.rootOf(gameBoard);
                rootPane.getChildren().add(nextBrickPanel);
                positionNextBrickPanel(scene);
                updateNextBrickPanel();
            }
        });
    }

    private void positionNextBrickPanel(Scene scene) {
        if (nextBrickPanel == null) return;
        if (panelPositioner != null) {
            panelPositioner.positionNextBrickPanel(nextBrickPanel, scene);
        }
    }

    private void updateNextBrickPanel() {
        if (nextBrickPanel != null && board != null) {
            nextBrickPanel.updateBricks(board.getNextBricks(5));
        }
    }

    private void initializeHoldBrickPanel() {
        holdBrickPanel = new HoldBrickPanel();
        Ui.run(() -> {
            Scene scene = SceneAccessor.sceneOf(gameBoard);
            if (scene != null) {
                Pane rootPane = SceneAccessor.rootOf(gameBoard);
                rootPane.getChildren().add(holdBrickPanel);
                positionHoldBrickPanel(scene);
                updateHoldBrickPanel();
            }
        });
    }

    private void positionHoldBrickPanel(Scene scene) {
        if (holdBrickPanel == null) return;
        if (panelPositioner != null) {
            panelPositioner.positionHoldBrickPanel(holdBrickPanel, scene);
        }
    }

    private void updateHoldBrickPanel() {
        if (holdBrickPanel != null && board != null) {
            holdBrickPanel.updateBrick(board.getHeldBrick());
        }
    }

    // Grid lines are drawn by GridRenderer.drawGridLines

    private void centerGameBoard(Scene scene) {
        LayoutHelper.centerGameBoard(scene, gameBoard);

        positionHoldBrickPanel(scene);
        positionNextBrickPanel(scene);
        if (statsPanel != null) {
            positionStatsPanel(scene);
        }
        if (statsPanelRight != null) {
            positionStatsPanelRight(scene);
        }
        // Menus are centered when shown via MenuController/MenuManager
    }

    private void initializeGameOverMenu() {
        if (menuController != null) menuController.initializeGameOverMenu(gameBoard);
    }

    private void initializePauseMenu() {
        if (menuController != null) menuController.initializePauseMenu(gameBoard);
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

    private void pauseGame() {
        if (gameLifecycle != null) {
            gameLifecycle.pauseTimers();
            isPause.setValue(Boolean.TRUE);
        }
        if (menuController != null) menuController.showPauseMenu(gameBoard);
    }

    private void resumeGame() {
        if (gameLifecycle != null) {
            gameLifecycle.resumeTimers();
            isPause.setValue(Boolean.FALSE);
        }
        if (menuController != null) menuController.hidePauseMenu();
        gamePanel.requestFocus();
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
        updateNextBrickPanel();

        gameState.resetLines();
        gameState.setGameStartTimeNow();
        statsUpdater.updateAllStats(gameState, board, statsPanel, statsPanelRight);
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

    private void initializeMainMenu() {
        if (menuController != null) menuController.initializeMainMenu(gameBoard);
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
        Ui.run(() -> statsUpdater.updateAllStats(gameState, board, statsPanel, statsPanelRight));

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

    private void initializeSettingsMenu() {
        if (menuController != null) menuController.initializeSettingsMenu(gameBoard);
        SettingsMenu sm = menuController != null ? menuController.getSettingsMenu() : null;
        if (sm != null) {
            sm.getMasterVolumeSlider().valueProperty().addListener((obs, oldVal, newVal) -> {
                audioManager.setMasterVolume(newVal.doubleValue());
            });
            sm.getMusicVolumeSlider().valueProperty().addListener((obs, oldVal, newVal) -> {
                audioManager.setMusicVolume(newVal.doubleValue());
            });
            sm.getSoundEffectVolumeSlider().valueProperty().addListener((obs, oldVal, newVal) -> {
                audioManager.setSoundEffectVolume(newVal.doubleValue());
            });
            sm.getMasterVolumeSlider().setValue(audioManager.getMasterVolume());
            sm.getMusicVolumeSlider().setValue(audioManager.getMusicVolume());
            sm.getSoundEffectVolumeSlider().setValue(audioManager.getSoundEffectVolume());
        }
    }

    public void showSettingsMenu() {
        if (menuController != null) menuController.showSettingsMenu(gameBoard);
    }

    public void hideSettingsMenu() {
        if (menuController != null) menuController.hideSettingsMenu();
    }

    private void initializeKeyBindingsMenu() {
        if (menuController != null) menuController.initializeKeyBindingsMenu(gameBoard);
    }

    public void showKeyBindingsMenu() {
        if (menuController != null) menuController.showKeyBindingsMenu(gameBoard);
        KeyBindingsMenu km = menuController != null ? menuController.getKeyBindingsMenu() : null;
        if (km != null) km.refreshBindings();
    }

    public void hideKeyBindingsMenu() {
        if (menuController != null) menuController.hideKeyBindingsMenu();
    }

    private void initializeThemeMenu() {
        if (menuController != null) menuController.initializeThemeMenu(gameBoard);
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
            refreshBrick(board.getViewData());
            refreshGameBackground(board.getBoardMatrix());
        }
        updateNextBrickPanel();
        updateHoldBrickPanel();
        if (ghostRenderer != null && board != null) {
            ghostRenderer.render(board.getGhostBrick(), boardCentered);
        }
    }

    public void setPrimaryStage(Stage stage) {
        this.primaryStage = stage;
    }

    private void positionBrickPanel(ViewData brick) {
        if (panelPositioner != null) {
            panelPositioner.positionBrickPanel(brickPanel, gamePanel, brick);
        }
    }

    // Initialize ghost brick panel (4x4 max size)
    // Ghost rendering handled by GhostRenderer

    private void initializeStatsPanel() {
        statsPanel = new StatsPanel();
        Ui.run(() -> {
            Scene scene = SceneAccessor.sceneOf(gameBoard);
            if (scene != null) {
                Pane rootPane = SceneAccessor.rootOf(gameBoard);
                rootPane.getChildren().add(statsPanel);
                positionStatsPanel(scene);
                updateStatsPanel();
            }
        });
    }

    private void positionStatsPanel(Scene scene) {
        if (statsPanel == null) return;
        if (panelPositioner != null) {
            panelPositioner.positionStatsPanel(statsPanel, scene);
        }
    }

    private void updateStatsPanel() {
        statsUpdater.updateAllStats(gameState, board, statsPanel, statsPanelRight);
    }

    private void updateStatsPanelRight() {
        statsUpdater.updateAllStats(gameState, board, statsPanel, statsPanelRight);
    }

    // Game timer: started via StatsUpdater and ticks are wired through GameLifecycle.initTimers

    private void initializeStatsPanelRight() {
        statsPanelRight = new StatsPanelRight();
        Ui.run(() -> {
            Scene scene = SceneAccessor.sceneOf(gameBoard);
            if (scene != null) {
                statsPanelRight.addToScene(scene);
                positionStatsPanelRight(scene);
                updateStatsPanelRight();
            }
        });
    }

    private void positionStatsPanelRight(Scene scene) {
        if (statsPanelRight == null) return;
        if (panelPositioner != null) {
            panelPositioner.positionStatsPanelRight(statsPanelRight, scene);
        }
    }
}