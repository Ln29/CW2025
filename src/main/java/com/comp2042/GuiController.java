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
    private static final int HIDDEN_ROW_COUNT = 2;

    @FXML
    private BorderPane gameBoard;

    @FXML
    private GridPane gamePanel;

    @FXML
    private Group groupNotification;

    @FXML
    private GridPane brickPanel;

    private PauseMenu pauseMenu;
    private MainMenu mainMenu;
    private SettingsMenu settingsMenu;
    private GameOverMenu gameOverMenu;
    private NextBrickPanel nextBrickPanel;
    private HoldBrickPanel holdBrickPanel;
    private GridPane ghostPanel;
    private Rectangle[][] ghostRectangles;
    private Board board;
    private Stage primaryStage;
    private StatsPanel statsPanel;
    private StatsPanelRight statsPanelRight;

    //stats track
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
        //prevent flashing at (0,0)
        brickPanel.setVisible(false);

        // Center the BorderPane in the root Pane
        Platform.runLater(() -> {
            Scene scene = gameBoard.getScene();
            if (scene != null) {
                centerGameBoard(scene);
                boardCentered = true;
                if (initialBrickData != null && rectangles != null && rectangles.length > 0) {
                    refreshBrick(initialBrickData);
                }
                if (nextBrickPanel == null) {
                    initializeNextBrickPanel();
                } else {
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
                if (code == KeyCode.ESCAPE && Boolean.FALSE.equals(isGameOver.getValue())) {
                    togglePauseMenu();
                    keyEvent.consume();
                    return;
                }
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
                if (Boolean.FALSE.equals(isPause.getValue()) && Boolean.FALSE.equals(isGameOver.getValue()) && eventListener != null) {
                    if (code == KeyCode.LEFT || code == KeyCode.A) {
                        refreshBrick(eventListener.onLeftEvent(new MoveEvent(EventType.LEFT, EventSource.USER)));
                        keyEvent.consume();
                    } else if (code == KeyCode.RIGHT || code == KeyCode.D) {
                        refreshBrick(eventListener.onRightEvent(new MoveEvent(EventType.RIGHT, EventSource.USER)));
                        keyEvent.consume();
                    } else if (code == KeyCode.UP || code == KeyCode.W) {
                        refreshBrick(eventListener.onRotateEvent(new MoveEvent(EventType.ROTATE, EventSource.USER)));
                        keyEvent.consume();
                    } else if (code == KeyCode.DOWN || code == KeyCode.S) {
                        moveDown(new MoveEvent(EventType.DOWN, EventSource.USER));
                        keyEvent.consume();
                    } else if (code == KeyCode.SPACE) {
                        hardDrop();
                        keyEvent.consume();
                    } else if (code == KeyCode.F || code == KeyCode.SHIFT) {
                        if (board != null && board.holdBrick()) {
                            refreshBrick(board.getViewData());
                            updateHoldBrickPanel();
                            updateNextBrickPanel();
                        }
                        keyEvent.consume();
                    }
                }
            }
        });
    }

    public void initGameView(int[][] boardMatrix, ViewData brick) {
        displayMatrix = new Rectangle[boardMatrix.length][boardMatrix[0].length];
        // Only render rows 2-21 (visible 20 rows), rows 0-1 are hidden
        for (int i = HIDDEN_ROW_COUNT; i <= 21 && i < boardMatrix.length; i++) {
            for (int j = 0; j < boardMatrix[i].length; j++) {
                Rectangle rectangle = new Rectangle(BRICK_SIZE, BRICK_SIZE);
                rectangle.setFill(Color.TRANSPARENT);
                displayMatrix[i][j] = rectangle;
                gamePanel.add(rectangle, j, i - HIDDEN_ROW_COUNT);
                rectangle.toFront();
            }
        }

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

        timeLine = new Timeline(new KeyFrame(
                Duration.millis(400),
                ae -> moveDown(new MoveEvent(EventType.DOWN, EventSource.THREAD))
        ));
        timeLine.setCycleCount(Timeline.INDEFINITE);

        lockDelayCheckTimeline = new Timeline(new KeyFrame(
                Duration.millis(50),
                ae -> checkLockDelay()
        ));
        lockDelayCheckTimeline.setCycleCount(Timeline.INDEFINITE);
        isPause.setValue(Boolean.TRUE);
    }



    private Paint getFillColor(int i) {
        Paint returnPaint;
        switch (i) {
            case 0:
                returnPaint = Color.TRANSPARENT;
                break;
            case 1:
                returnPaint = Color.AQUA;
                break;
            case 2:
                returnPaint = Color.BLUEVIOLET;
                break;
            case 3:
                returnPaint = Color.DARKGREEN;
                break;
            case 4:
                returnPaint = Color.YELLOW;
                break;
            case 5:
                returnPaint = Color.RED;
                break;
            case 6:
                returnPaint = Color.BEIGE;
                break;
            case 7:
                returnPaint = Color.BURLYWOOD;
                break;
            default:
                returnPaint = Color.WHITE;
                break;
        }
        return returnPaint;
    }

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
            //only position and show brick if board is centered
            if (boardCentered && gameBoard.getLayoutX() > 0) {
                positionBrickPanel(brick);
                brickPanel.setVisible(true);
                brickPanel.toFront();
            } else {
                //Hide brick until board is properly positioned
                brickPanel.setVisible(false);
            }
            //hide cells that are in hidden rows
            int brickY = brick.getyPosition();
            for (int i = 0; i < brick.getBrickData().length; i++) {
                for (int j = 0; j < brick.getBrickData()[i].length; j++) {
                    int cellValue = brick.getBrickData()[i][j];
                    int boardRow = brickY + i;
                    //only show cells that are in visible rows
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
            // Check if lock delay has expired
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
                // Update high score if current score exceeds it
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

        // Reset game stats
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
        // Add to root pane
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

        double boardWidth = 300 + 24; // 324px
        double boardHeight = 600 + 24; // 624px
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
        double boardHeight = 600 + 24; // 624px
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

        //vertical lines
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

        //horizontal lines
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
        // Board dimensions: 10 columns × 30px = 300px, 20 rows × 30px = 600px
        // Add border width (12px on each side = 24px total)
        double boardWidth = 300 + 24; // 324px
        double boardHeight = 600 + 24; // 624px

        // Center the BorderPane in the scene
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
            restartGame();
        });

        gameOverMenu.setOnMainMenu(() -> {
            restartGame();
            hideMainMenu(); //hidden first
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
            resumeGame();
        });

        pauseMenu.setOnRestart(() -> {
            restartGame();
        });

        pauseMenu.setOnMainMenu(() -> {
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

        // Set up button callbacks
        mainMenu.setOnStart(() -> {
            startGame();
        });

        mainMenu.setOnSettings(() -> {
            showSettingsMenu();
        });

        mainMenu.setOnExit(() -> {
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

        settingsMenu.setOnKeyBindings(() -> {
            // to do
        });

        settingsMenu.setOnThemeSelection(() -> {
            // to do
        });

        settingsMenu.setOnBack(() -> {
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

    private void refreshGhostBrick() {
        if (board == null || ghostPanel == null || ghostRectangles == null) {
            return;
        }

        GhostBrick ghostBrick = board.getGhostBrick();

        if (ghostBrick == null) {
            ghostPanel.setVisible(false);
            return;
        }

        //position
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

        //update ghost brick cells
        int[][] ghostData = ghostBrick.getBrickData();
        int ghostY = ghostBrick.getyPosition();

        //clear all ghost rectangles first
        for (int i = 0; i < ghostRectangles.length; i++) {
            for (int j = 0; j < ghostRectangles[i].length; j++) {
                ghostRectangles[i][j].setFill(Color.TRANSPARENT);
                ghostRectangles[i][j].setStroke(null);
            }
        }

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
        //background < ghost < brick
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

        double boardWidth = 300 + 24; // 324px
        double boardHeight = 600 + 24; // 624px
        double boardX = gameBoard.getLayoutX();
        double boardY = gameBoard.getLayoutY();

        statsPanel.position(boardX, boardY, boardWidth, boardHeight);
    }

    private void updateStatsPanel() {
        if (statsPanel == null) return;

        // Update lines
        statsPanel.updateLines(totalLinesCleared);

        // Update level (placeholder - showing 1 for now)
        statsPanel.updateLevel(1);

        // Update high score
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
                Duration.millis(1000), // Update every second
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

        double boardWidth = 300 + 24; // 324px
        double boardHeight = 600 + 24; // 624px
        double boardX = gameBoard.getLayoutX();
        double boardY = gameBoard.getLayoutY();

        statsPanelRight.position(boardX, boardY, boardWidth, boardHeight);
    }
}