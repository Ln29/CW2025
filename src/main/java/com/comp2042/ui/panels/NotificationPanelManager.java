package com.comp2042.ui.panels;

import com.comp2042.controller.AudioManager;
import com.comp2042.config.GameConstants;
import com.comp2042.ui.NotificationService;
import javafx.scene.Group;
import javafx.scene.Scene;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.Pane;

/**
 * Manages notification panel groups (combo, score, center) and their positioning.
 * Extracted from GuiController to follow the same pattern as PanelManager.
 */
public class NotificationPanelManager {
    private final Group comboNotificationGroup;
    private final Group scoreNotificationGroup;
    private final Group centerNotificationGroup;
    private final NotificationService notificationService;
    private BorderPane gameBoard;

    /**
     * Creates a NotificationPanelManager with the given notification groups.
     * 
     * @param audioManager Audio manager for notification service
     * @param comboNotificationGroup Group for combo notifications
     * @param scoreNotificationGroup Group for score bonus notifications
     * @param centerNotificationGroup Group for center notifications (level up)
     */
    public NotificationPanelManager(AudioManager audioManager,
                                   Group comboNotificationGroup,
                                   Group scoreNotificationGroup,
                                   Group centerNotificationGroup) {
        this.comboNotificationGroup = comboNotificationGroup;
        this.scoreNotificationGroup = scoreNotificationGroup;
        this.centerNotificationGroup = centerNotificationGroup;
        this.notificationService = new NotificationService(
            audioManager,
            comboNotificationGroup,
            scoreNotificationGroup,
            centerNotificationGroup,
            GameConstants.NOTIFICATION_MAX
        );
    }

    /**
     * Sets the game board reference for positioning calculations.
     * 
     * @param gameBoard The game board BorderPane
     */
    public void setGameBoard(BorderPane gameBoard) {
        this.gameBoard = gameBoard;
    }

    /**
     * Moves notification groups from root to the gameplay layer.
     * 
     * @param gameplayLayer The gameplay layer group
     * @param root The root pane containing the groups
     */
    public void addToGameplayLayer(Group gameplayLayer, Pane root) {
        if (root == null || gameplayLayer == null) return;
        
        if (root.getChildren().contains(comboNotificationGroup)) {
            root.getChildren().remove(comboNotificationGroup);
            gameplayLayer.getChildren().add(comboNotificationGroup);
        }
        if (root.getChildren().contains(scoreNotificationGroup)) {
            root.getChildren().remove(scoreNotificationGroup);
            gameplayLayer.getChildren().add(scoreNotificationGroup);
        }
        if (root.getChildren().contains(centerNotificationGroup)) {
            root.getChildren().remove(centerNotificationGroup);
            gameplayLayer.getChildren().add(centerNotificationGroup);
        }
    }

    /**
     * Positions the notification groups relative to the game board.
     * 
     * @param scene The scene containing the game board
     */
    public void positionGroups(Scene scene) {
        if (scene == null || gameBoard == null || 
            comboNotificationGroup == null || scoreNotificationGroup == null) {
            return;
        }

        double boardWidth = GameConstants.BOARD_COLS * GameConstants.BRICK_SIZE + 
                           GameConstants.BOARD_BORDER_TOTAL_PX;
        double boardHeight = GameConstants.BOARD_VISIBLE_ROWS * GameConstants.BRICK_SIZE + 
                            GameConstants.BOARD_BORDER_TOTAL_PX;
        double boardX = gameBoard.getLayoutX();
        double boardY = gameBoard.getLayoutY();

        // Calculate StatsPanelRight position
        double nextPanelX = boardX + boardWidth + 30;
        double nextPanelHeight = boardHeight / 2;
        double statsPanelRightY = boardY + nextPanelHeight + 200;

        double comboX = nextPanelX;
        double comboY = statsPanelRightY - 250;

        double scoreX = nextPanelX;
        double scoreY = statsPanelRightY - 200;

        comboNotificationGroup.setLayoutX(comboX);
        comboNotificationGroup.setLayoutY(comboY);

        scoreNotificationGroup.setLayoutX(scoreX);
        scoreNotificationGroup.setLayoutY(scoreY);

        // Position center notification group in the middle
        if (centerNotificationGroup != null) {
            double centerX = boardX + boardWidth / 2;
            double centerY = boardY + boardHeight / 2;
            centerNotificationGroup.setLayoutX(centerX - 110);
            centerNotificationGroup.setLayoutY(centerY - 100);
        }
    }

    /**
     * Gets the notification service instance.
     * 
     * @return The NotificationService instance
     */
    public NotificationService getNotificationService() {
        return notificationService;
    }

    /**
     * Resets the notification service (combo and level tracking).
     */
    public void reset() {
        if (notificationService != null) {
            notificationService.resetCombo();
            notificationService.resetLevelTracking();
        }
    }
}

