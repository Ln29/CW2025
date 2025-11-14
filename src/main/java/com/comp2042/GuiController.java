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
import javafx.scene.effect.Reflection;
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

    @FXML
    private GameOverPanel gameOverPanel;

    private NextBrickPanel nextBrickPanel;
    private HoldBrickPanel holdBrickPanel;
    private Board board;

    private boolean boardCentered = false;
    private ViewData initialBrickData = null;

    private Rectangle[][] displayMatrix;

    private InputEventListener eventListener;

    private Rectangle[][] rectangles;

    private Timeline timeLine;

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
                centerNotificationGroup(scene);
                boardCentered = true;
                if (initialBrickData != null && rectangles != null && rectangles.length > 0) {
                    refreshBrick(initialBrickData);
                }
                scene.widthProperty().addListener((obs, oldVal, newVal) -> {
                    centerGameBoard(scene);
                    centerNotificationGroup(scene);
                    // Reposition brick after centering - refreshBrick no yet done
                });
                scene.heightProperty().addListener((obs, oldVal, newVal) -> {
                    centerGameBoard(scene);
                    centerNotificationGroup(scene);
                    // Reposition brick after centering - not yet done
                });

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
            }
        });

        gamePanel.setFocusTraversable(true);
        gamePanel.requestFocus();
        gamePanel.setOnKeyPressed(new EventHandler<KeyEvent>() {
            @Override
            public void handle(KeyEvent keyEvent) {
                if (Boolean.FALSE.equals(isPause.getValue()) && Boolean.FALSE.equals(isGameOver.getValue()) && eventListener != null) {
                    KeyCode code = keyEvent.getCode();
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
                    } else if (code == KeyCode.F || code == KeyCode.SHIFT) {
                        if (board != null && board.holdBrick()) {
                            refreshBrick(board.getViewData());
                            updateHoldBrickPanel();
                            updateNextBrickPanel();
                        }
                        keyEvent.consume();
                    }
                }
                if (keyEvent.getCode() == KeyCode.N) {
                    newGame(null);
                }
            }
        });
        gameOverPanel.setVisible(false);

        final Reflection reflection = new Reflection();
        reflection.setFraction(0.8);
        reflection.setTopOpacity(0.9);
        reflection.setTopOffset(-12);
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

        drawGridLines();

        timeLine = new Timeline(new KeyFrame(
                Duration.millis(400),
                ae -> moveDown(new MoveEvent(EventType.DOWN, EventSource.THREAD))
        ));
        timeLine.setCycleCount(Timeline.INDEFINITE);
        timeLine.play();
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
                NotificationPanel notificationPanel = new NotificationPanel("+" + downData.getClearRow().getScoreBonus());
                if (groupNotification.getChildren().size() > 5) {
                    groupNotification.getChildren().remove(0);
                }
                groupNotification.getChildren().add(notificationPanel);
                notificationPanel.showScore(groupNotification.getChildren());
            }
            refreshBrick(downData.getViewData());
            updateNextBrickPanel();
            if (board != null) {
                board.resetHoldUsage();
            }
        }
        gamePanel.requestFocus();
    }

    public void setEventListener(InputEventListener eventListener) {
        this.eventListener = eventListener;
        // After setting event listener, if board is already centered, refresh brick position
        if (boardCentered && eventListener != null && rectangles != null && rectangles.length > 0) {
            Platform.runLater(() -> {
                // trigger a refresh by getting current ViewData
                // not yet done
            });
        }
    }

    public void setBoard(Board board) {
        this.board = board;
    }

    public void bindScore(IntegerProperty integerProperty) {
    }

    public void gameOver() {
        timeLine.stop();
        gameOverPanel.setVisible(true);
        isGameOver.setValue(Boolean.TRUE);
        Platform.runLater(() -> {
            Scene scene = gameOverPanel.getScene();
            if (scene != null) {
                centerNotificationGroup(scene);
            }
        });
    }

    public void newGame(ActionEvent actionEvent) {
        timeLine.stop();
        gameOverPanel.setVisible(false);
        eventListener.createNewGame();
        updateNextBrickPanel();
        gamePanel.requestFocus();
        timeLine.play();
        isPause.setValue(Boolean.FALSE);
        isGameOver.setValue(Boolean.FALSE);
    }

    public void pauseGame(ActionEvent actionEvent) {
        if (timeLine == null) {
            return;
        }
        if (Boolean.TRUE.equals(isPause.getValue())) {
            timeLine.play();
            isPause.setValue(Boolean.FALSE);
        } else {
            timeLine.stop();
            isPause.setValue(Boolean.TRUE);
        }
        gamePanel.requestFocus();
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

        double panelX = boardX + boardWidth + 10;
        double panelY = boardY;
        double panelHeight = boardHeight / 2;

        nextBrickPanel.setLayoutX(panelX);
        nextBrickPanel.setLayoutY(panelY);
        nextBrickPanel.setPrefHeight(panelHeight);
        nextBrickPanel.setMinHeight(panelHeight);
        nextBrickPanel.setMaxHeight(panelHeight);
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

        double boardHeight = 600 + 24; // 624px
        double boardX = gameBoard.getLayoutX();
        double boardY = gameBoard.getLayoutY();

        double panelX = boardX - 95;
        double panelY = boardY;
        double panelHeight = boardHeight / 4;

        holdBrickPanel.setLayoutX(panelX);
        holdBrickPanel.setLayoutY(panelY);
        holdBrickPanel.setPrefHeight(panelHeight);
        holdBrickPanel.setMinHeight(panelHeight);
        holdBrickPanel.setMaxHeight(panelHeight);
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
    }

    private void centerNotificationGroup(Scene scene) {
        // Center game over panel
        double boardCenterX = gameBoard.getLayoutX() + (300 + 24) / 2;
        double boardCenterY = gameBoard.getLayoutY() + (600 + 24) / 2;

        double panelWidth = gameOverPanel.getBoundsInLocal().getWidth();
        double panelHeight = gameOverPanel.getBoundsInLocal().getHeight();

        // If bounds not available use default size
        if (panelWidth == 0) {
            panelWidth = 200; // Default width
        }
        if (panelHeight == 0) {
            panelHeight = 100; // Default height
        }

        groupNotification.setLayoutX(boardCenterX - panelWidth / 2);
        groupNotification.setLayoutY(boardCenterY - panelHeight / 2);
    }

    private void positionBrickPanel(ViewData brick) {
        double x = gameBoard.getLayoutX() + gamePanel.getLayoutX() + brick.getxPosition() * (brickPanel.getVgap() + BRICK_SIZE);
        double cellSize = brickPanel.getHgap() + BRICK_SIZE;
        double y = gameBoard.getLayoutY() + gamePanel.getLayoutY() + (brick.getyPosition() - HIDDEN_ROW_COUNT) * cellSize;

        brickPanel.setLayoutX(x);
        brickPanel.setLayoutY(y);
    }
}