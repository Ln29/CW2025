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
import javafx.application.Platform;
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

    private static final int BRICK_SIZE = 30;
    private static final int HIDDEN_ROW_COUNT = 2; // Top 2 rows are hidden

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
    private KeyBindingsConfig keyBindingsConfig;
    private ThemeConfig themeConfig;
    private AudioManager audioManager;
    private NextBrickPanel nextBrickPanel;
    private HoldBrickPanel holdBrickPanel;
    private Stage primaryStage;
    private StatsPanel statsPanel;
    private StatsPanelRight statsPanelRight;
    private GridPane ghostPanel;
    private Rectangle[][] ghostRectangles;
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

        // Center game board after scene is ready
        Platform.runLater(() -> {
            Scene scene = gameBoard.getScene();
            if (scene != null) {
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

                // Route events to active menu
                if (Boolean.TRUE.equals(isPause.getValue()) && pauseMenu != null && pauseMenu.isVisible()) {
                    pauseMenu.fireEvent(keyEvent);
                    keyEvent.consume();
                    return;
                }

                if (Boolean.TRUE.equals(isGameOver.getValue()) && gameOverMenu != null && gameOverMenu.isVisible()) {
                    gameOverMenu.fireEvent(keyEvent);
                    keyEvent.consume();
                    return;
                }

                if (settingsMenu != null && settingsMenu.isVisible()) {
                    settingsMenu.fireEvent(keyEvent);
                    keyEvent.consume();
                    return;
                }

                if (keyBindingsMenu != null && keyBindingsMenu.isVisible()) {
                    keyBindingsMenu.fireEvent(keyEvent);
                    keyEvent.consume();
                    return;
                }

                if (themeMenu != null && themeMenu.isVisible()) {
                    themeMenu.fireEvent(keyEvent);
                    keyEvent.consume();
                    return;
                }

                if (mainMenu != null && mainMenu.isVisible()) {
                    mainMenu.fireEvent(keyEvent);
                    keyEvent.consume();
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

        initializeGhostPanel(brick.getBrickData().length, brick.getBrickData()[0].length);
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

    // Get semi-transparent color for ghost brick outline
    private Paint getGhostStrokeColor(int i) {
        Paint baseColor = getFillColor(i);
        if (baseColor == Color.TRANSPARENT) {
            return Color.TRANSPARENT;
        }
        if (baseColor instanceof Color) {
            Color color = (Color) baseColor;
            return new Color(color.getRed(), color.getGreen(), color.getBlue(), 0.8);
        }
        return baseColor;
    }

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

            refreshGhostBrick();
        }
    }

    public void refreshGameBackground(int[][] board) {
        for (int i = HIDDEN_ROW_COUNT; i <= 21 && i < board.length; i++) {
            for (int j = 0; j < board[i].length; j++) {
                setRectangleData(board[i][j], displayMatrix[i][j]);
            }
        }
    }

    private void setRectangleData(int color, Rectangle rectangle) {
        rectangle.setFill(getFillColor(color));
        rectangle.setArcHeight(9);
        rectangle.setArcWidth(9);
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
        timeLine.stop();
        if (lockDelayCheckTimeline != null) {
            lockDelayCheckTimeline.stop();
        }
        if (gameTimeTimeline != null) {
            gameTimeTimeline.stop();
        }
        isGameOver.setValue(Boolean.TRUE);

        if (audioManager != null) {
            audioManager.stopAllMusic();
            audioManager.playSoundEffect("lose.wav");
        }

        if (gameOverMenu == null) {
            initializeGameOverMenu();
        }

        gameOverMenu.setVisible(true);
        Platform.runLater(() -> {
            Scene scene = gameBoard.getScene();
            if (scene != null) {
                gameOverMenu.toFront();
                centerGameOverMenu(scene);
                gameOverMenu.requestFocusForNavigation();
            }
        });
    }

    public void newGame(ActionEvent actionEvent) {
        timeLine.stop();
        if (lockDelayCheckTimeline != null) {
            lockDelayCheckTimeline.stop();
        }
        if (gameTimeTimeline != null) {
            gameTimeTimeline.stop();
        }
        eventListener.createNewGame();
        updateNextBrickPanel();

        totalLinesCleared = 0;
        gameStartTime = System.currentTimeMillis();
        updateStatsPanel();
        updateStatsPanelRight();
        startGameTimer();

        gamePanel.requestFocus();
        timeLine.play();
        if (lockDelayCheckTimeline != null) {
            lockDelayCheckTimeline.play();
        }
        isPause.setValue(Boolean.FALSE);
        isGameOver.setValue(Boolean.FALSE);
    }

    public void pauseGame(ActionEvent actionEvent) {
        togglePauseMenu();
    }

    private void initializeNextBrickPanel() {
        nextBrickPanel = new NextBrickPanel();
        Platform.runLater(() -> {
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

        double boardWidth = 300 + 24;
        double boardHeight = 600 + 24;
        double boardX = gameBoard.getLayoutX();
        double boardY = gameBoard.getLayoutY();

        nextBrickPanel.position(boardX, boardY, boardWidth, boardHeight);
    }

    private void updateNextBrickPanel() {
        if (nextBrickPanel != null && board != null) {
            nextBrickPanel.updateBricks(board.getNextBricks(5));
        }
    }

    private void initializeHoldBrickPanel() {
        holdBrickPanel = new HoldBrickPanel();
        Platform.runLater(() -> {
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

        double boardWidth = 300 + 24;
        double boardHeight = 600 + 24;
        double boardX = gameBoard.getLayoutX();
        double boardY = gameBoard.getLayoutY();

        holdBrickPanel.position(boardX, boardY, boardWidth, boardHeight);
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
        double boardWidth = 300 + 24;
        double boardHeight = 600 + 24;

        double centerX = (scene.getWidth() - boardWidth) / 2;
        double centerY = (scene.getHeight() - boardHeight) / 2;

        gameBoard.setLayoutX(centerX);
        gameBoard.setLayoutY(centerY);

        positionHoldBrickPanel(scene);
        positionNextBrickPanel(scene);
        if (statsPanel != null) {
            positionStatsPanel(scene);
        }
        if (statsPanelRight != null) {
            positionStatsPanelRight(scene);
        }
        if (pauseMenu != null) {
            centerPauseMenu(scene);
        }
        if (gameOverMenu != null) {
            centerGameOverMenu(scene);
        }
        if (mainMenu != null) {
            centerMainMenu(scene);
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

        Platform.runLater(() -> {
            Scene scene = gameBoard.getScene();
            if (scene != null) {
                Pane rootPane = (Pane) scene.getRoot();
                rootPane.getChildren().add(gameOverMenu);
                gameOverMenu.toFront();
                centerGameOverMenu(scene);
            }
        });
    }

    private void centerGameOverMenu(Scene scene) {
        if (gameOverMenu == null) return;

        double boardCenterX = gameBoard.getLayoutX() + (300 + 24) / 2;
        double boardCenterY = gameBoard.getLayoutY() + (600 + 24) / 2;

        gameOverMenu.setLayoutX(boardCenterX - gameOverMenu.getPrefWidth() / 2);
        gameOverMenu.setLayoutY(boardCenterY - gameOverMenu.getPrefHeight() / 2);
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

        Platform.runLater(() -> {
            Scene scene = gameBoard.getScene();
            if (scene != null) {
                Pane rootPane = (Pane) scene.getRoot();
                rootPane.getChildren().add(pauseMenu);
                pauseMenu.toFront();
                centerPauseMenu(scene);
            }
        });
    }

    private void centerPauseMenu(Scene scene) {
        if (pauseMenu == null) return;

        double boardCenterX = gameBoard.getLayoutX() + (300 + 24) / 2;
        double boardCenterY = gameBoard.getLayoutY() + (600 + 24) / 2;

        pauseMenu.setLayoutX(boardCenterX - pauseMenu.getPrefWidth() / 2);
        pauseMenu.setLayoutY(boardCenterY - pauseMenu.getPrefHeight() / 2);
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
        timeLine.stop();
        if (lockDelayCheckTimeline != null) {
            lockDelayCheckTimeline.stop();
        }
        if (gameTimeTimeline != null) {
            gameTimeTimeline.stop();
        }
        isPause.setValue(Boolean.TRUE);

        if (audioManager != null) {
            audioManager.pauseAllMusic();
        }

        if (pauseMenu == null) {
            initializePauseMenu();
        }

        pauseMenu.setVisible(true);
        Platform.runLater(() -> {
            Scene scene = gameBoard.getScene();
            if (scene != null) {
                pauseMenu.toFront();
                centerPauseMenu(scene);
                pauseMenu.requestFocusForNavigation();
            }
        });
    }

    private void resumeGame() {
        timeLine.play();
        if (lockDelayCheckTimeline != null) {
            lockDelayCheckTimeline.play();
        }
        if (gameTimeTimeline != null) {
            gameTimeTimeline.play();
        }
        isPause.setValue(Boolean.FALSE);

        if (audioManager != null) {
            audioManager.resumeAllMusic();
        }

        if (pauseMenu != null) {
            pauseMenu.setVisible(false);
        }

        gamePanel.requestFocus();
    }

    private void restartGame() {
        timeLine.stop();
        if (lockDelayCheckTimeline != null) {
            lockDelayCheckTimeline.stop();
        }
        if (gameTimeTimeline != null) {
            gameTimeTimeline.stop();
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

        gamePanel.requestFocus();
        timeLine.play();
        if (lockDelayCheckTimeline != null) {
            lockDelayCheckTimeline.play();
        }
        isPause.setValue(Boolean.FALSE);
        isGameOver.setValue(Boolean.FALSE);
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

        Platform.runLater(() -> {
            Scene scene = gameBoard.getScene();
            if (scene != null) {
                Pane rootPane = (Pane) scene.getRoot();
                if (rootPane.getChildren().contains(mainMenu)) {
                    rootPane.getChildren().remove(mainMenu);
                }
                rootPane.getChildren().add(mainMenu);
                mainMenu.toFront();
                centerMainMenu(scene);
            }
        });
    }

    private void centerMainMenu(Scene scene) {
        if (mainMenu == null) return;

        double sceneWidth = scene.getWidth();
        double sceneHeight = scene.getHeight();

        mainMenu.setLayoutX((sceneWidth - mainMenu.getPrefWidth()) / 2);
        mainMenu.setLayoutY((sceneHeight - mainMenu.getPrefHeight()) / 2);
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

        mainMenu.setVisible(true);
        Platform.runLater(() -> {
            Scene scene = gameBoard.getScene();
            if (scene != null) {
                Pane rootPane = (Pane) scene.getRoot();
                if (rootPane.getChildren().contains(mainMenu)) {
                    rootPane.getChildren().remove(mainMenu);
                }
                rootPane.getChildren().add(mainMenu);
                mainMenu.toFront();
                centerMainMenu(scene);
                mainMenu.requestFocusForNavigation();
            }
        });
    }

    public void hideMainMenu() {
        if (mainMenu != null) {
            mainMenu.setVisible(false);
        }
    }

    private void startGame() {
        hideMainMenu();

        if (audioManager != null) {
            String musicFile = (themeConfig != null && themeConfig.getMusicFile() != null)
                    ? themeConfig.getMusicFile()
                    : "default.wav";
            audioManager.playGameMusic(musicFile);
        }

        isPause.setValue(Boolean.FALSE);
        if (timeLine != null) {
            timeLine.play();
        }
        if (lockDelayCheckTimeline != null) {
            lockDelayCheckTimeline.play();
        }
        startGameTimer();

        totalLinesCleared = 0;
        gameStartTime = System.currentTimeMillis();
        updateStatsPanel();
        updateStatsPanelRight();

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

        Platform.runLater(() -> {
            Scene scene = gameBoard.getScene();
            if (scene != null) {
                Pane rootPane = (Pane) scene.getRoot();
                if (rootPane.getChildren().contains(settingsMenu)) {
                    rootPane.getChildren().remove(settingsMenu);
                }
                rootPane.getChildren().add(settingsMenu);
                settingsMenu.toFront();
                centerSettingsMenu(scene);
            }
        });
    }

    private void centerSettingsMenu(Scene scene) {
        if (settingsMenu == null) return;

        double sceneWidth = scene.getWidth();
        double sceneHeight = scene.getHeight();

        settingsMenu.setLayoutX((sceneWidth - settingsMenu.getPrefWidth()) / 2);
        settingsMenu.setLayoutY((sceneHeight - settingsMenu.getPrefHeight()) / 2);
    }

    public void showSettingsMenu() {
        if (settingsMenu == null) {
            initializeSettingsMenu();
        }

        settingsMenu.setVisible(true);
        Platform.runLater(() -> {
            Scene scene = gameBoard.getScene();
            if (scene != null) {
                Pane rootPane = (Pane) scene.getRoot();
                if (rootPane.getChildren().contains(settingsMenu)) {
                    rootPane.getChildren().remove(settingsMenu);
                }
                rootPane.getChildren().add(settingsMenu);
                settingsMenu.toFront();
                centerSettingsMenu(scene);
                settingsMenu.requestFocusForNavigation();
            }
        });
    }

    public void hideSettingsMenu() {
        if (settingsMenu != null) {
            settingsMenu.setVisible(false);
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

        Platform.runLater(() -> {
            Scene scene = gameBoard.getScene();
            if (scene != null) {
                Pane rootPane = (Pane) scene.getRoot();
                if (rootPane.getChildren().contains(keyBindingsMenu)) {
                    rootPane.getChildren().remove(keyBindingsMenu);
                }
                rootPane.getChildren().add(keyBindingsMenu);
                keyBindingsMenu.toFront();
                centerKeyBindingsMenu(scene);
            }
        });
    }

    private void centerKeyBindingsMenu(Scene scene) {
        if (keyBindingsMenu == null) return;

        double sceneWidth = scene.getWidth();
        double sceneHeight = scene.getHeight();

        keyBindingsMenu.setLayoutX((sceneWidth - keyBindingsMenu.getPrefWidth()) / 2);
        keyBindingsMenu.setLayoutY((sceneHeight - keyBindingsMenu.getPrefHeight()) / 2);
    }

    public void showKeyBindingsMenu() {
        if (keyBindingsMenu == null) {
            initializeKeyBindingsMenu();
        }

        keyBindingsMenu.setVisible(true);
        keyBindingsMenu.refreshBindings();
        Platform.runLater(() -> {
            Scene scene = gameBoard.getScene();
            if (scene != null) {
                Pane rootPane = (Pane) scene.getRoot();
                if (rootPane.getChildren().contains(keyBindingsMenu)) {
                    rootPane.getChildren().remove(keyBindingsMenu);
                }
                rootPane.getChildren().add(keyBindingsMenu);
                keyBindingsMenu.toFront();
                centerKeyBindingsMenu(scene);
                keyBindingsMenu.requestFocusForNavigation();
            }
        });
    }

    public void hideKeyBindingsMenu() {
        if (keyBindingsMenu != null) {
            keyBindingsMenu.setVisible(false);
        }
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
            if (themeConfig != null) {
                themeConfig.setTheme(theme);
                updateBackgroundImage();
                if (audioManager != null) {
                    audioManager.playGameMusic(theme.getMusicFile());
                }
                refreshAllBrickDisplays();
            }
        });

        Platform.runLater(() -> {
            Scene scene = gameBoard.getScene();
            if (scene != null) {
                Pane rootPane = (Pane) scene.getRoot();
                if (rootPane.getChildren().contains(themeMenu)) {
                    rootPane.getChildren().remove(themeMenu);
                }
                rootPane.getChildren().add(themeMenu);
                themeMenu.toFront();
                centerThemeMenu(scene);
            }
        });
    }

    private void centerThemeMenu(Scene scene) {
        if (themeMenu == null) return;

        double sceneWidth = scene.getWidth();
        double sceneHeight = scene.getHeight();

        themeMenu.setLayoutX((sceneWidth - themeMenu.getPrefWidth()) / 2);
        themeMenu.setLayoutY((sceneHeight - themeMenu.getPrefHeight()) / 2);
    }

    public void showThemeMenu() {
        if (themeMenu == null) {
            initializeThemeMenu();
        }

        themeMenu.setVisible(true);
        Platform.runLater(() -> {
            Scene scene = gameBoard.getScene();
            if (scene != null) {
                Pane rootPane = (Pane) scene.getRoot();
                if (rootPane.getChildren().contains(themeMenu)) {
                    rootPane.getChildren().remove(themeMenu);
                }
                rootPane.getChildren().add(themeMenu);
                themeMenu.toFront();
                centerThemeMenu(scene);
                themeMenu.requestFocusForNavigation();
            }
        });
    }

    public void hideThemeMenu() {
        if (themeMenu != null) {
            themeMenu.setVisible(false);
        }
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
                    Platform.runLater(() -> {
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

    private void refreshAllBrickDisplays() {
        if (board != null) {
            refreshBrick(board.getViewData());
            refreshGameBackground(board.getBoardMatrix());
        }
        updateNextBrickPanel();
        updateHoldBrickPanel();
        refreshGhostBrick();
    }

    public void setPrimaryStage(Stage stage) {
        this.primaryStage = stage;
    }

    private void positionBrickPanel(ViewData brick) {
        double x = gameBoard.getLayoutX() + gamePanel.getLayoutX() + brick.getxPosition() * (brickPanel.getVgap() + BRICK_SIZE);
        double cellSize = brickPanel.getHgap() + BRICK_SIZE;
        double y = gameBoard.getLayoutY() + gamePanel.getLayoutY() + (brick.getyPosition() - HIDDEN_ROW_COUNT) * cellSize;

        brickPanel.setLayoutX(x);
        brickPanel.setLayoutY(y);
    }

    // Initialize ghost brick panel (4x4 max size)
    private void initializeGhostPanel(int rows, int cols) {
        ghostPanel = new GridPane();
        ghostPanel.setHgap(brickPanel.getHgap());
        ghostPanel.setVgap(brickPanel.getVgap());
        ghostPanel.setVisible(false);
        ghostPanel.setMouseTransparent(true);

        int maxSize = 4;
        ghostRectangles = new Rectangle[maxSize][maxSize];
        for (int i = 0; i < maxSize; i++) {
            for (int j = 0; j < maxSize; j++) {
                Rectangle rectangle = new Rectangle(BRICK_SIZE, BRICK_SIZE);
                rectangle.setFill(Color.TRANSPARENT);
                rectangle.setArcHeight(9);
                rectangle.setArcWidth(9);
                ghostRectangles[i][j] = rectangle;
                ghostPanel.add(rectangle, j, i);
            }
        }

        Platform.runLater(() -> {
            Scene scene = gameBoard.getScene();
            if (scene != null) {
                Pane rootPane = (Pane) scene.getRoot();
                rootPane.getChildren().add(ghostPanel);
                ghostPanel.toBack();
                brickPanel.toFront();
            }
        });
    }

    // Update ghost brick display
    private void refreshGhostBrick() {
        if (board == null || ghostPanel == null || ghostRectangles == null) {
            return;
        }

        GhostBrick ghostBrick = board.getGhostBrick();

        if (ghostBrick == null) {
            ghostPanel.setVisible(false);
            return;
        }

        // Position ghost panel
        if (boardCentered && gameBoard.getLayoutX() > 0) {
            double x = gameBoard.getLayoutX() + gamePanel.getLayoutX() + ghostBrick.getxPosition() * (ghostPanel.getVgap() + BRICK_SIZE);
            double cellSize = ghostPanel.getHgap() + BRICK_SIZE;
            double y = gameBoard.getLayoutY() + gamePanel.getLayoutY() + (ghostBrick.getyPosition() - HIDDEN_ROW_COUNT) * cellSize;

            ghostPanel.setLayoutX(x);
            ghostPanel.setLayoutY(y);
            ghostPanel.setVisible(true);
            ghostPanel.toBack();
        } else {
            ghostPanel.setVisible(false);
        }

        int[][] ghostData = ghostBrick.getBrickData();
        int ghostY = ghostBrick.getyPosition();

        // Clear all ghost cells
        for (int i = 0; i < ghostRectangles.length; i++) {
            for (int j = 0; j < ghostRectangles[i].length; j++) {
                ghostRectangles[i][j].setFill(Color.TRANSPARENT);
                ghostRectangles[i][j].setStroke(null);
            }
        }

        // Draw ghost outline
        for (int i = 0; i < ghostData.length && i < ghostRectangles.length; i++) {
            for (int j = 0; j < ghostData[i].length && j < ghostRectangles[i].length; j++) {
                int cellValue = ghostData[i][j];
                int boardRow = ghostY + i;

                if (boardRow >= HIDDEN_ROW_COUNT && cellValue != 0) {
                    ghostRectangles[i][j].setFill(Color.TRANSPARENT);
                    ghostRectangles[i][j].setStroke(getGhostStrokeColor(cellValue));
                    ghostRectangles[i][j].setStrokeWidth(2.0);
                    ghostRectangles[i][j].setArcHeight(9);
                    ghostRectangles[i][j].setArcWidth(9);
                }
            }
        }

        ghostPanel.toBack();
        brickPanel.toFront();
    }

    private void initializeStatsPanel() {
        statsPanel = new StatsPanel();
        Platform.runLater(() -> {
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

        double boardWidth = 300 + 24;
        double boardHeight = 600 + 24;
        double boardX = gameBoard.getLayoutX();
        double boardY = gameBoard.getLayoutY();

        statsPanel.position(boardX, boardY, boardWidth, boardHeight);
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
        Platform.runLater(() -> {
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

        double boardWidth = 300 + 24;
        double boardHeight = 600 + 24;
        double boardX = gameBoard.getLayoutX();
        double boardY = gameBoard.getLayoutY();

        statsPanelRight.position(boardX, boardY, boardWidth, boardHeight);
    }
}