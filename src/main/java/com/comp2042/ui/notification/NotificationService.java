package com.comp2042.ui.notification;

import com.comp2042.audio.AudioManager;
import com.comp2042.config.GameConstants;
import com.comp2042.ui.Ui;
import javafx.scene.Group;

public class NotificationService {
    private final AudioManager audioManager;
    private final Group comboNotificationGroup;
    private final Group scoreNotificationGroup;
    private final Group centerNotificationGroup;
    private final int maxChildren;

    private int comboCount = -1;
    private NotificationPanel currentComboPanel = null;
    private int previousLevel = -1;

    public NotificationService(AudioManager audioManager, Group comboNotificationGroup, Group scoreNotificationGroup, Group centerNotificationGroup, int maxChildren) {
        this.audioManager = audioManager;
        this.comboNotificationGroup = comboNotificationGroup;
        this.scoreNotificationGroup = scoreNotificationGroup;
        this.centerNotificationGroup = centerNotificationGroup;
        this.maxChildren = maxChildren;
    }

    public void showCombo(int combo) {
        Ui.run(() -> {
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

    public void clearCombo() {
        Ui.run(() -> {
            if (currentComboPanel != null && comboNotificationGroup != null) {
                currentComboPanel.fadeOutAndRemove(comboNotificationGroup.getChildren());
                currentComboPanel = null;
            }
        });
    }

    public void showScoreBonus(int bonus) {
        Ui.run(() -> {
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

    public int getComboCount() {
        return comboCount;
    }

    public void onLinesCleared(int linesRemoved, int bonus) {
        if (linesRemoved > 0) {
            comboCount++;
            if (audioManager != null) {
                audioManager.playSoundEffect(GameConstants.SFX_CLEAR_LINE);
            }
            showCombo(comboCount);
            // Calculate total bonus (base bonus + combo reward) for display
            // comboCount starts from -1, so first clear (comboCount = 0) gives no combo bonus
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

    public void resetCombo() {
        if (comboCount > 0) {
            clearCombo();
        }
        comboCount = -1;
    }

    public void checkLevelUp(int currentLevel) {
        if (previousLevel != -1 && currentLevel > previousLevel) {
            showLevelUp(currentLevel);
        }
        previousLevel = currentLevel;
    }

    public void showLevelUp(int level) {
        Ui.run(() -> {
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

    public void resetLevelTracking() {
        previousLevel = -1;
    }
}