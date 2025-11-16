package com.comp2042;

import javafx.animation.KeyFrame;
import javafx.animation.Timeline;
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
import javafx.scene.shape.Line;
import javafx.scene.shape.Rectangle;
import javafx.scene.text.Font;
import javafx.stage.Stage;
import javafx.util.Duration;

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

    private PauseMenu pauseMenu;
    private GameOverMenu gameOverMenu;
    private MainMenu mainMenu;
    private SettingsMenu settingsMenu;
    private KeyBindingsMenu keyBindingsMenu;
    private ThemeMenu themeMenu;
    private MenuManager menuManager;
    private KeyBindingsConfig keyBindingsConfig;
    private ThemeConfig themeConfig;
    private AudioManager audioManager;
    private GameLifecycle gameLifecycle;
    private PanelPositioner panelPositioner;
    private GridRenderer gridRenderer = new GridRenderer();
    private ThemeApplier themeApplier;
    private InputRouter.Overlay activeOverlay = InputRouter.Overlay.NONE;
    private NextBrickPanel nextBrickPanel;
    private HoldBrickPanel holdBrickPanel;
    private Stage primaryStage;
    private StatsPanel statsPanel;
    private StatsPanelRight statsPanelRight;
    private GhostRenderer ghostRenderer;
    private Board board;

    // Game stats
    private int totalLinesCleared = 0;
    private int highScore = 0;
    private long gameStartTime = 0;
    private Timeline gameTimeTimeline;

    private boolean boardCentered = false;
    private ViewData initialBrickData = null;

    private Rectangle[][] displayMatrix;
    private InputEventListener eventListener;
    private Rectangle[][] rectangles;
    private Timeline timeLine;
    private Timeline lockDelayCheckTimeline;

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
        updateBackgroundImage();
        audioManager = AudioManager.getInstance();
        gameLifecycle = new GameLifecycle(audioManager);
        themeApplier = new ThemeApplier(themeConfig, audioManager);

        // Center game board after scene is ready
        Ui.run(() -> {
            Scene scene = gameBoard.getScene();
            if (scene != null) {
                Pane rootPane = (Pane) scene.getRoot();
                menuManager = new MenuManager(rootPane, gameBoard);
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
        gamePanel.setOnKeyPressed(new EventHandler<KeyEvent>() {
            @Override
            public void handle(KeyEvent keyEvent) {
                KeyCode code = keyEvent.getCode();

                // Handle pause key
                KeyBindingsConfig.Action pauseAction = keyBindingsConfig != null ? keyBindingsConfig.getAction(code) : null;
                if ((code == KeyCode.ESCAPE || (pauseAction == KeyBindingsConfig.Action.PAUSE)) && Boolean.FALSE.equals(isGameOver.getValue())) {
                    if (keyBindingsMenu == null || !keyBindingsMenu.isVisible()) {
                        togglePauseMenu();
                        keyEvent.consume();
                        return;
                    }
                }

                // Route events to active overlay
                if (InputRouter.route(
                        activeOverlay,
                        keyEvent,
                        mainMenu,
                        settingsMenu,
                        keyBindingsMenu,
                        themeMenu,
                        pauseMenu,
                        gameOverMenu)) {
                    return;
                }

                // Handle game controls
                if (Boolean.FALSE.equals(isPause.getValue()) && Boolean.FALSE.equals(isGameOver.getValue()) && eventListener != null) {
                    KeyBindingsConfig.Action action = keyBindingsConfig.getAction(code);
                    if (action != null) {
                        switch (action) {
                            case MOVE_LEFT:
                                refreshBrick(eventListener.onLeftEvent(new MoveEvent(EventType.LEFT, EventSource.USER)));
                                keyEvent.consume();
                                break;
                            case MOVE_RIGHT:
                                refreshBrick(eventListener.onRightEvent(new MoveEvent(EventType.RIGHT, EventSource.USER)));
                                keyEvent.consume();
                                break;
                            case ROTATE:
                                refreshBrick(eventListener.onRotateEvent(new MoveEvent(EventType.ROTATE, EventSource.USER)));
                                keyEvent.consume();
                                break;
                            case SOFT_DROP:
                                moveDown(new MoveEvent(EventType.DOWN, EventSource.USER));
                                keyEvent.consume();
                                break;
                            case HARD_DROP:
                                hardDrop();
                                keyEvent.consume();
                                break;
                            case HOLD:
                                if (board != null && board.holdBrick()) {
                                    refreshBrick(board.getViewData());
                                    updateHoldBrickPanel();
                                    updateNextBrickPanel();
                                }
                                keyEvent.consume();
                                break;
                            case PAUSE:
                                togglePauseMenu();
                                keyEvent.consume();
                                break;
                        }
                    }
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
        drawGridLines();

        // Main game loop
        timeLine = new Timeline(new KeyFrame(
                Duration.millis(400),
                ae -> moveDown(new MoveEvent(EventType.DOWN, EventSource.THREAD))
        ));
        timeLine.setCycleCount(Timeline.INDEFINITE);

        // Lock delay checker
        lockDelayCheckTimeline = new Timeline(new KeyFrame(
                Duration.millis(50),
                ae -> checkLockDelay()
        ));
        lockDelayCheckTimeline.setCycleCount(Timeline.INDEFINITE);

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
                if (downData.getClearRow().getLinesRemoved() > 0) {
                    totalLinesCleared += downData.getClearRow().getLinesRemoved();
                    updateStatsPanel();
                    NotificationPanel notificationPanel = new NotificationPanel("+" + downData.getClearRow().getScoreBonus());
                    if (groupNotification.getChildren().size() > 5) {
                        groupNotification.getChildren().remove(0);
                    }
                    groupNotification.getChildren().add(notificationPanel);
                    notificationPanel.showScore(groupNotification.getChildren());
                    if (audioManager != null) {
                        audioManager.playSoundEffect("clearline.wav");
                    }
                } else {
                    if (audioManager != null) {
                        audioManager.playSoundEffect("blockfall.wav");
                    }
                }
            }
            refreshBrick(downData.getViewData());
            updateNextBrickPanel();
            updateStatsPanel();
            updateStatsPanelRight();
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
                if (downData.getClearRow().getLinesRemoved() > 0) {
                    totalLinesCleared += downData.getClearRow().getLinesRemoved();
                    updateStatsPanel();
                    NotificationPanel notificationPanel = new NotificationPanel("+" + downData.getClearRow().getScoreBonus());
                    if (groupNotification.getChildren().size() > 5) {
                        groupNotification.getChildren().remove(0);
                    }
                    groupNotification.getChildren().add(notificationPanel);
                    notificationPanel.showScore(groupNotification.getChildren());
                    if (audioManager != null) {
                        audioManager.playSoundEffect("clearline.wav");
                    }
                } else {
                    if (audioManager != null) {
                        audioManager.playSoundEffect("blockfall.wav");
                    }
                }
            }
            refreshBrick(downData.getViewData());
            updateNextBrickPanel();
            updateStatsPanel();
            updateStatsPanelRight();
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
                    totalLinesCleared += downData.getClearRow().getLinesRemoved();
                    updateStatsPanel();
                    NotificationPanel notificationPanel = new NotificationPanel("+" + downData.getClearRow().getScoreBonus());
                    if (groupNotification.getChildren().size() > 5) {
                        groupNotification.getChildren().remove(0);
                    }
                    groupNotification.getChildren().add(notificationPanel);
                    notificationPanel.showScore(groupNotification.getChildren());
                }
                refreshBrick(downData.getViewData());
                updateNextBrickPanel();
                updateStatsPanelRight();
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
                updateStatsPanelRight();
                updateStatsPanel();
                if (newVal.intValue() > highScore) {
                    highScore = newVal.intValue();
                    updateStatsPanel();
                }
            });
        }
    }

    public void gameOver() {
        if (gameLifecycle != null) {
            gameLifecycle.gameOver(timeLine, lockDelayCheckTimeline, gameTimeTimeline, isGameOver);
        }

        if (gameOverMenu == null) {
            initializeGameOverMenu();
        }

        Ui.run(() -> {
            Scene scene = gameBoard.getScene();
            if (scene != null) {
                if (menuManager == null) {
                    menuManager = new MenuManager((Pane) scene.getRoot(), gameBoard);
                }
                menuManager.showCenteredOnBoard(gameOverMenu);
                gameOverMenu.requestFocusForNavigation();
            }
        });
    }

    public void newGame(ActionEvent actionEvent) {
        if (gameLifecycle != null) {
            gameLifecycle.stop(timeLine, lockDelayCheckTimeline, gameTimeTimeline);
        }
        eventListener.createNewGame();
        updateNextBrickPanel();

        totalLinesCleared = 0;
        gameStartTime = System.currentTimeMillis();
        updateStatsPanel();
        updateStatsPanelRight();
        startGameTimer();

        if (gameLifecycle != null) {
            gameLifecycle.start(timeLine, lockDelayCheckTimeline, gameTimeTimeline, isPause, isGameOver);
        }
        gamePanel.requestFocus();
    }

    public void pauseGame(ActionEvent actionEvent) {
        togglePauseMenu();
    }

    private void initializeNextBrickPanel() {
        nextBrickPanel = new NextBrickPanel();
        Ui.run(() -> {
            Scene scene = gameBoard.getScene();
            if (scene != null) {
                Pane rootPane = (Pane) scene.getRoot();
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
            Scene scene = gameBoard.getScene();
            if (scene != null) {
                Pane rootPane = (Pane) scene.getRoot();
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

    private void drawGridLines() {
        Color gridColor = Color.rgb(128, 128, 128, 0.5);

        double hgap = gamePanel.getHgap();
        double vgap = gamePanel.getVgap();
        double cellWidth = BRICK_SIZE + hgap;
        double cellHeight = BRICK_SIZE + vgap;
        double totalWidth = 10 * BRICK_SIZE + 9 * hgap;
        double totalHeight = 20 * BRICK_SIZE + 19 * vgap;

        // Vertical lines
        for (int i = 0; i <= 10; i++) {
            Line verticalLine = new Line();
            double x = i * cellWidth;
            verticalLine.setStartX(x);
            verticalLine.setStartY(0);
            verticalLine.setEndX(x);
            verticalLine.setEndY(totalHeight);
            verticalLine.setStroke(gridColor);
            verticalLine.setStrokeWidth(1);
            verticalLine.setManaged(false);
            gamePanel.getChildren().add(0, verticalLine);
            verticalLine.toBack();
        }

        // Horizontal lines
        for (int i = 0; i <= 20; i++) {
            Line horizontalLine = new Line();
            double y = i * cellHeight;
            horizontalLine.setStartX(0);
            horizontalLine.setStartY(y);
            horizontalLine.setEndX(totalWidth);
            horizontalLine.setEndY(y);
            horizontalLine.setStroke(gridColor);
            horizontalLine.setStrokeWidth(1);
            horizontalLine.setManaged(false);
            gamePanel.getChildren().add(0, horizontalLine);
            horizontalLine.toBack();
        }
    }

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
        if (pauseMenu != null) {
            if (menuManager == null) {
                Pane rootPane = (Pane) scene.getRoot();
                menuManager = new MenuManager(rootPane, gameBoard);
            }
            menuManager.centerOnBoard(pauseMenu);
        }
        if (gameOverMenu != null) {
            if (menuManager == null) {
                Pane rootPane = (Pane) scene.getRoot();
                menuManager = new MenuManager(rootPane, gameBoard);
            }
            menuManager.centerOnBoard(gameOverMenu);
        }
        if (mainMenu != null) {
            if (menuManager == null) {
                Pane rootPane = (Pane) scene.getRoot();
                menuManager = new MenuManager(rootPane, gameBoard);
            }
            menuManager.centerOnScene(mainMenu, scene);
        }
    }

    private void initializeGameOverMenu() {
        gameOverMenu = new GameOverMenu();
        gameOverMenu.setVisible(false);

        gameOverMenu.setOnRestart(() -> {
            if (audioManager != null) {
                audioManager.playSoundEffect("click.wav");
            }
            restartGame();
        });

        gameOverMenu.setOnMainMenu(() -> {
            if (audioManager != null) {
                audioManager.playSoundEffect("click.wav");
            }
            restartGame();
            hideMainMenu();
            showMainMenu();
        });

        Ui.run(() -> {
            Scene scene = gameBoard.getScene();
            if (scene != null) {
                Pane rootPane = (Pane) scene.getRoot();
                if (menuManager == null) {
                    menuManager = new MenuManager(rootPane, gameBoard);
                }
                menuManager.ensureOnTop(gameOverMenu);
                menuManager.centerOnBoard(gameOverMenu);
            }
        });
    }

    private void initializePauseMenu() {
        pauseMenu = new PauseMenu();
        pauseMenu.setVisible(false);

        pauseMenu.setOnResume(() -> {
            if (audioManager != null) {
                audioManager.playSoundEffect("click.wav");
            }
            resumeGame();
        });

        pauseMenu.setOnRestart(() -> {
            if (audioManager != null) {
                audioManager.playSoundEffect("click.wav");
            }
            restartGame();
        });

        pauseMenu.setOnMainMenu(() -> {
            if (audioManager != null) {
                audioManager.playSoundEffect("click.wav");
            }
            restartGame();
            hideMainMenu();
            showMainMenu();
        });

        Ui.run(() -> {
            Scene scene = gameBoard.getScene();
            if (scene != null) {
                Pane rootPane = (Pane) scene.getRoot();
                if (menuManager == null) {
                    menuManager = new MenuManager(rootPane, gameBoard);
                }
                menuManager.ensureOnTop(pauseMenu);
                menuManager.centerOnBoard(pauseMenu);
            }
        });
    }



    private void togglePauseMenu() {
        if (timeLine == null) {
            return;
        }

        if (Boolean.TRUE.equals(isPause.getValue())) {
            resumeGame();
        } else {
            pauseGame();
        }
    }

    private void pauseGame() {
        if (gameLifecycle != null) {
            gameLifecycle.pause(timeLine, lockDelayCheckTimeline, gameTimeTimeline, isPause);
        }

        if (pauseMenu == null) {
            initializePauseMenu();
        }

        Ui.run(() -> {
            Scene scene = gameBoard.getScene();
            if (scene != null) {
                if (menuManager == null) {
                    menuManager = new MenuManager((Pane) scene.getRoot(), gameBoard);
                }
                menuManager.showCenteredOnBoard(pauseMenu);
                pauseMenu.requestFocusForNavigation();
            }
        });
    }

    private void resumeGame() {
        if (gameLifecycle != null) {
            gameLifecycle.resume(timeLine, lockDelayCheckTimeline, gameTimeTimeline, isPause);
        }

        if (pauseMenu != null) {
            pauseMenu.setVisible(false);
        }

        gamePanel.requestFocus();
    }

    private void restartGame() {
        if (gameLifecycle != null) {
            gameLifecycle.stop(timeLine, lockDelayCheckTimeline, gameTimeTimeline);
        }

        if (pauseMenu != null) {
            pauseMenu.setVisible(false);
        }

        if (gameOverMenu != null) {
            gameOverMenu.setVisible(false);
        }
        eventListener.createNewGame();
        updateNextBrickPanel();

        totalLinesCleared = 0;
        gameStartTime = System.currentTimeMillis();
        updateStatsPanel();
        updateStatsPanelRight();
        startGameTimer();

        if (audioManager != null) {
            String musicFile = (themeConfig != null && themeConfig.getMusicFile() != null)
                    ? themeConfig.getMusicFile()
                    : "default.wav";
            audioManager.playGameMusic(musicFile);
        }

        if (gameLifecycle != null) {
            gameLifecycle.start(timeLine, lockDelayCheckTimeline, gameTimeTimeline, isPause, isGameOver);
        }
        gamePanel.requestFocus();
    }

    private void initializeMainMenu() {
        mainMenu = new MainMenu();
        mainMenu.setVisible(false);

        mainMenu.setOnStart(() -> {
            if (audioManager != null) {
                audioManager.playSoundEffect("click.wav");
            }
            startGame();
        });

        mainMenu.setOnSettings(() -> {
            if (audioManager != null) {
                audioManager.playSoundEffect("click.wav");
            }
            showSettingsMenu();
        });

        mainMenu.setOnExit(() -> {
            if (audioManager != null) {
                audioManager.playSoundEffect("click.wav");
            }
            exitGame();
        });

        Ui.run(() -> {
            Scene scene = gameBoard.getScene();
            if (scene != null) {
                Pane rootPane = (Pane) scene.getRoot();
                if (menuManager == null) {
                    menuManager = new MenuManager(rootPane, gameBoard);
                }
                menuManager.ensureOnTop(mainMenu);
                menuManager.centerOnScene(mainMenu, scene);
            }
        });
    }

    public void showMainMenu() {
        if (mainMenu == null) {
            initializeMainMenu();
        }

        isPause.setValue(Boolean.TRUE);
        if (timeLine != null) {
            timeLine.stop();
        }
        if (lockDelayCheckTimeline != null) {
            lockDelayCheckTimeline.stop();
        }
        if (gameTimeTimeline != null) {
            gameTimeTimeline.stop();
        }

        if (brickPanel != null) {
            brickPanel.setVisible(false);
        }

        if (audioManager != null) {
            audioManager.playMainMenuMusic();
        }

        Ui.run(() -> {
            Scene scene = gameBoard.getScene();
            if (scene != null) {
                Pane rootPane = (Pane) scene.getRoot();
                if (menuManager == null) {
                    menuManager = new MenuManager(rootPane, gameBoard);
                }
                menuManager.showCenteredOnScene(mainMenu, scene);
                activeOverlay = InputRouter.Overlay.MAIN_MENU;
                mainMenu.requestFocusForNavigation();
            }
        });
    }

    public void hideMainMenu() {
        if (mainMenu != null) {
            mainMenu.setVisible(false);
        }
        activeOverlay = InputRouter.Overlay.NONE;
    }

    private void startGame() {
        hideMainMenu();

        if (audioManager != null) {
            String musicFile = (themeConfig != null && themeConfig.getMusicFile() != null)
                    ? themeConfig.getMusicFile()
                    : "default.wav";
            audioManager.playGameMusic(musicFile);
        }

        startGameTimer();

        totalLinesCleared = 0;
        gameStartTime = System.currentTimeMillis();
        updateStatsPanel();
        updateStatsPanelRight();

        if (gameLifecycle != null) {
            gameLifecycle.start(timeLine, lockDelayCheckTimeline, gameTimeTimeline, isPause, isGameOver);
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
        settingsMenu = new SettingsMenu();
        settingsMenu.setVisible(false);

        // Link volume sliders to audio manager
        settingsMenu.getMasterVolumeSlider().valueProperty().addListener((obs, oldVal, newVal) -> {
            audioManager.setMasterVolume(newVal.doubleValue());
        });
        settingsMenu.getMusicVolumeSlider().valueProperty().addListener((obs, oldVal, newVal) -> {
            audioManager.setMusicVolume(newVal.doubleValue());
        });
        settingsMenu.getSoundEffectVolumeSlider().valueProperty().addListener((obs, oldVal, newVal) -> {
            audioManager.setSoundEffectVolume(newVal.doubleValue());
        });

        settingsMenu.getMasterVolumeSlider().setValue(audioManager.getMasterVolume());
        settingsMenu.getMusicVolumeSlider().setValue(audioManager.getMusicVolume());
        settingsMenu.getSoundEffectVolumeSlider().setValue(audioManager.getSoundEffectVolume());

        settingsMenu.setOnKeyBindings(() -> {
            if (audioManager != null) {
                audioManager.playSoundEffect("click.wav");
            }
            hideSettingsMenu();
            showKeyBindingsMenu();
        });

        settingsMenu.setOnThemeSelection(() -> {
            if (audioManager != null) {
                audioManager.playSoundEffect("click.wav");
            }
            hideSettingsMenu();
            showThemeMenu();
        });

        settingsMenu.setOnBack(() -> {
            if (audioManager != null) {
                audioManager.playSoundEffect("click.wav");
            }
            hideSettingsMenu();
        });

        Ui.run(() -> {
            Scene scene = gameBoard.getScene();
            if (scene != null) {
                Pane rootPane = (Pane) scene.getRoot();
                if (menuManager == null) {
                    menuManager = new MenuManager(rootPane, gameBoard);
                }
                menuManager.ensureOnTop(settingsMenu);
                menuManager.centerOnScene(settingsMenu, scene);
            }
        });
    }

    public void showSettingsMenu() {
        if (settingsMenu == null) {
            initializeSettingsMenu();
        }

        Ui.run(() -> {
            Scene scene = gameBoard.getScene();
            if (scene != null) {
                Pane rootPane = (Pane) scene.getRoot();
                if (menuManager == null) {
                    menuManager = new MenuManager(rootPane, gameBoard);
                }
                menuManager.showCenteredOnScene(settingsMenu, scene);
                activeOverlay = InputRouter.Overlay.SETTINGS;
                settingsMenu.requestFocusForNavigation();
            }
        });
    }

    public void hideSettingsMenu() {
        if (settingsMenu != null) {
            settingsMenu.setVisible(false);
        }
        if (mainMenu != null && mainMenu.isVisible()) {
            activeOverlay = InputRouter.Overlay.MAIN_MENU;
        } else {
            activeOverlay = InputRouter.Overlay.NONE;
        }
    }

    private void initializeKeyBindingsMenu() {
        keyBindingsMenu = new KeyBindingsMenu();
        keyBindingsMenu.setVisible(false);

        keyBindingsMenu.setOnBack(() -> {
            if (audioManager != null) {
                audioManager.playSoundEffect("click.wav");
            }
            hideKeyBindingsMenu();
            showSettingsMenu();
        });

        keyBindingsMenu.setOnBindingsChanged(() -> {
            // Key bindings changed
        });

        Ui.run(() -> {
            Scene scene = gameBoard.getScene();
            if (scene != null) {
                Pane rootPane = (Pane) scene.getRoot();
                if (menuManager == null) {
                    menuManager = new MenuManager(rootPane, gameBoard);
                }
                menuManager.ensureOnTop(keyBindingsMenu);
                menuManager.centerOnScene(keyBindingsMenu, scene);
            }
        });
    }

    public void showKeyBindingsMenu() {
        if (keyBindingsMenu == null) {
            initializeKeyBindingsMenu();
        }

        keyBindingsMenu.setVisible(true);
        keyBindingsMenu.refreshBindings();
        Ui.run(() -> {
            Scene scene = gameBoard.getScene();
            if (scene != null) {
                Pane rootPane = (Pane) scene.getRoot();
                if (menuManager == null) {
                    menuManager = new MenuManager(rootPane, gameBoard);
                }
                menuManager.showCenteredOnScene(keyBindingsMenu, scene);
                activeOverlay = InputRouter.Overlay.KEY_BINDINGS;
                keyBindingsMenu.requestFocusForNavigation();
            }
        });
    }

    public void hideKeyBindingsMenu() {
        if (keyBindingsMenu != null) {
            keyBindingsMenu.setVisible(false);
        }
        activeOverlay = InputRouter.Overlay.SETTINGS;
    }

    private void initializeThemeMenu() {
        themeMenu = new ThemeMenu();
        themeMenu.setVisible(false);

        themeMenu.setOnBack(() -> {
            if (audioManager != null) {
                audioManager.playSoundEffect("click.wav");
            }
            hideThemeMenu();
            showSettingsMenu();
            if (audioManager != null && (Boolean.TRUE.equals(isPause.getValue()) || Boolean.TRUE.equals(isGameOver.getValue()))) {
                audioManager.playMainMenuMusic();
            }
        });

        themeMenu.setOnThemeSelected((theme) -> {
            if (audioManager != null) {
                audioManager.playSoundEffect("click.wav");
            }
            applyTheme(theme);
        });

        Ui.run(() -> {
            Scene scene = gameBoard.getScene();
            if (scene != null) {
                Pane rootPane = (Pane) scene.getRoot();
                if (menuManager == null) {
                    menuManager = new MenuManager(rootPane, gameBoard);
                }
                menuManager.ensureOnTop(themeMenu);
                menuManager.centerOnScene(themeMenu, scene);
            }
        });
    }

    public void showThemeMenu() {
        if (themeMenu == null) {
            initializeThemeMenu();
        }

        Ui.run(() -> {
            Scene scene = gameBoard.getScene();
            if (scene != null) {
                Pane rootPane = (Pane) scene.getRoot();
                if (menuManager == null) {
                    menuManager = new MenuManager(rootPane, gameBoard);
                }
                menuManager.showCenteredOnScene(themeMenu, scene);
                activeOverlay = InputRouter.Overlay.THEME;
                themeMenu.requestFocusForNavigation();
            }
        });
    }

    public void hideThemeMenu() {
        if (themeMenu != null) {
            themeMenu.setVisible(false);
        }
        activeOverlay = InputRouter.Overlay.SETTINGS;
    }

    private void updateBackgroundImage() {
        if (themeConfig != null) {
            String imagePath = themeConfig.getBackgroundImage();
            if (imagePath != null) {
                try {
                    javafx.scene.image.Image bgImage = new javafx.scene.image.Image(
                            getClass().getClassLoader().getResource("assets/images/" + imagePath).toExternalForm()
                    );
                    javafx.scene.layout.BackgroundImage backgroundImage = new javafx.scene.layout.BackgroundImage(
                            bgImage,
                            javafx.scene.layout.BackgroundRepeat.NO_REPEAT,
                            javafx.scene.layout.BackgroundRepeat.NO_REPEAT,
                            javafx.scene.layout.BackgroundPosition.CENTER,
                            new javafx.scene.layout.BackgroundSize(javafx.scene.layout.BackgroundSize.AUTO, javafx.scene.layout.BackgroundSize.AUTO, false, false, true, true)
                    );
                    Ui.run(() -> {
                        Scene scene = gameBoard.getScene();
                        if (scene != null) {
                            Pane rootPane = (Pane) scene.getRoot();
                            rootPane.setBackground(new javafx.scene.layout.Background(backgroundImage));
                        }
                    });
                } catch (Exception e) {
                    System.err.println("Error updating background image: " + e.getMessage());
                }
            }
        }
    }

    // Single entry point for applying a theme consistently
    private void applyTheme(ThemeMenu.Theme theme) {
        Scene scene = gameBoard != null ? gameBoard.getScene() : null;
        boolean playGameMusic = true;
        if (themeApplier != null) {
            themeApplier.apply(theme, scene, this::refreshAllBrickDisplays, playGameMusic);
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
            Scene scene = gameBoard.getScene();
            if (scene != null) {
                Pane rootPane = (Pane) scene.getRoot();
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
        if (statsPanel == null) return;

        statsPanel.updateLines(totalLinesCleared);
        statsPanel.updateLevel(1);
        statsPanel.updateHighScore(highScore);
    }

    private void updateStatsPanelRight() {
        if (statsPanelRight != null && board != null) {
            int currentScore = board.getScore().scoreProperty().getValue();
            statsPanelRight.updateScore(currentScore);
        }
    }

    private void startGameTimer() {
        if (gameTimeTimeline != null) {
            gameTimeTimeline.stop();
        }

        gameStartTime = System.currentTimeMillis();
        gameTimeTimeline = new Timeline(new KeyFrame(
                Duration.millis(1000),
                ae -> updateGameTime()
        ));
        gameTimeTimeline.setCycleCount(Timeline.INDEFINITE);
        gameTimeTimeline.play();
    }

    private void updateGameTime() {
        if (statsPanel == null || gameStartTime == 0) return;

        long elapsed = System.currentTimeMillis() - gameStartTime;
        long seconds = elapsed / 1000;
        long minutes = seconds / 60;
        seconds = seconds % 60;

        String timeString = String.format("%02d:%02d", minutes, seconds);
        statsPanel.updateTime(timeString);
    }

    private void initializeStatsPanelRight() {
        statsPanelRight = new StatsPanelRight();
        Ui.run(() -> {
            Scene scene = gameBoard.getScene();
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