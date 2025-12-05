package com.comp2042.ui;

import com.comp2042.controller.AudioManager;
import com.comp2042.config.GameConstants;
import com.comp2042.ui.panels.NotificationPanel;
import com.comp2042.ui.util.PlatformUtils;
import javafx.scene.Group;

/**
 * Service for displaying in-game notifications including combos, score bonuses, and level ups.
 * Manages notification panels and their animations.
 */
public class NotificationService {
    private final AudioManager audioManager;
    private final Group comboNotificationGroup;
    private final Group scoreNotificationGroup;
    private final Group centerNotificationGroup;
    private final int maxChildren;

    private int comboCount = -1;
    private NotificationPanel currentComboPanel = null;
    private int previousLevel = -1;

    /**
     * Creates a notification service for managing game notifications.
     * 
     * @param audioManager audio manager for playing sound effects
     * @param comboNotificationGroup group for combo notifications
     * @param scoreNotificationGroup group for score bonus notifications
     * @param centerNotificationGroup group for center notifications (e.g., level up)
     * @param maxChildren maximum number of notification children per group
     */
    public NotificationService(AudioManager audioManager, Group comboNotificationGroup, Group scoreNotificationGroup, Group centerNotificationGroup, int maxChildren) {
        this.audioManager = audioManager;
        this.comboNotificationGroup = comboNotificationGroup;
        this.scoreNotificationGroup = scoreNotificationGroup;
        this.centerNotificationGroup = centerNotificationGroup;
        this.maxChildren = maxChildren;
    }

    /**
     * Shows or updates the combo notification.
     * 
     * @param combo the combo count to display (0 to hide)
     */
    public void showCombo(int combo) {
        PlatformUtils.run(() -> {
            if (comboNotificationGroup == null || combo <= 0) return;

            if (combo == 0) {
                if (currentComboPanel != null) {
                    currentComboPanel.fadeOutAndRemove(comboNotificationGroup.getChildren());
                    currentComboPanel = null;
                }
                return;
            }

            if (currentComboPanel == null) {
                currentComboPanel = new NotificationPanel(combo + " COMBO !!", true);
                currentComboPanel.setLayoutX(0);
                currentComboPanel.setLayoutY(0);
                comboNotificationGroup.getChildren().add(currentComboPanel);
            } else {
                currentComboPanel.updateText(combo + " COMBO !!");
            }
        });
    }

    /**
     * Clears the current combo notification.
     */
    public void clearCombo() {
        PlatformUtils.run(() -> {
            if (currentComboPanel != null && comboNotificationGroup != null) {
                currentComboPanel.fadeOutAndRemove(comboNotificationGroup.getChildren());
                currentComboPanel = null;
            }
        });
    }

    /**
     * Shows a score bonus notification.
     * 
     * @param bonus the bonus score to display
     */
    public void showScoreBonus(int bonus) {
        PlatformUtils.run(() -> {
            if (scoreNotificationGroup == null) return;
            if (scoreNotificationGroup.getChildren().size() > maxChildren) {
                scoreNotificationGroup.getChildren().remove(0);
            }
            NotificationPanel panel = new NotificationPanel("+" + bonus, false);
            panel.setLayoutX(0);
            panel.setLayoutY(0);
            scoreNotificationGroup.getChildren().add(panel);
            panel.showScore(scoreNotificationGroup.getChildren());
        });
    }

    /**
     * Gets the current combo count.
     * 
     * @return the combo count (-1 if no combo active)
     */
    public int getComboCount() {
        return comboCount;
    }

    /**
     * Handles line clear events, updating combos and showing notifications.
     * 
     * @param linesRemoved number of lines cleared
     * @param bonus base score bonus from line clear
     */
    public void onLinesCleared(int linesRemoved, int bonus) {
        if (linesRemoved > 0) {
            comboCount++;
            if (audioManager != null) {
                audioManager.playSoundEffect(GameConstants.SFX_CLEAR_LINE);
            }
            showCombo(comboCount);
            // comboCount starts from -1, 0 gives no combo bonus
            int totalBonus = bonus;
            if (comboCount > 0) {
                int comboReward = comboCount * 50;
                totalBonus = bonus + comboReward;
            }
            showScoreBonus(totalBonus);
        } else {
            if (comboCount > 0) {
                clearCombo();
            }
            comboCount = -1;
            if (audioManager != null) {
                audioManager.playSoundEffect(GameConstants.SFX_BLOCK_FALL);
            }
        }
    }

    /**
     * Resets the combo counter and clears combo notification.
     */
    public void resetCombo() {
        if (comboCount > 0) {
            clearCombo();
        }
        comboCount = -1;
    }

    /**
     * Checks if level has increased and shows level up notification if needed.
     * 
     * @param currentLevel the current game level
     */
    public void checkLevelUp(int currentLevel) {
        if (previousLevel != -1 && currentLevel > previousLevel) {
            showLevelUp(currentLevel);
        }
        previousLevel = currentLevel;
    }

    /**
     * Shows a level up notification.
     * 
     * @param level the new level number
     */
    public void showLevelUp(int level) {
        PlatformUtils.run(() -> {
            if (centerNotificationGroup == null) return;
            if (centerNotificationGroup.getChildren().size() > maxChildren) {
                centerNotificationGroup.getChildren().remove(0);
            }
            NotificationPanel panel = new NotificationPanel("LEVEL " + level + " !!", false, true);
            panel.setLayoutX(0);
            panel.setLayoutY(0);
            centerNotificationGroup.getChildren().add(panel);
            panel.showLevelUp(centerNotificationGroup.getChildren());
        });
    }

    /**
     * Resets level tracking to initial state.
     */
    public void resetLevelTracking() {
        previousLevel = -1;
    }
}

