package com.comp2042.ui.notification;

import com.comp2042.audio.AudioManager;
import com.comp2042.config.GameConstants;
import com.comp2042.ui.Ui;
import javafx.scene.Group;

public class NotificationService {
    private final AudioManager audioManager;
    private final Group comboNotificationGroup;
    private final Group scoreNotificationGroup;
    private final int maxChildren;

    private int comboCount = 0;
    private NotificationPanel currentComboPanel = null;

    public NotificationService(AudioManager audioManager, Group comboNotificationGroup, Group scoreNotificationGroup, int maxChildren) {
        this.audioManager = audioManager;
        this.comboNotificationGroup = comboNotificationGroup;
        this.scoreNotificationGroup = scoreNotificationGroup;
        this.maxChildren = maxChildren;
    }

    public void showCombo(int combo) {
        Ui.run(() -> {
            if (comboNotificationGroup == null || combo <= 0) return;

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

    public void onLinesCleared(int linesRemoved, int bonus) {
        if (linesRemoved > 0) {
            comboCount++;
            if (audioManager != null) {
                audioManager.playSoundEffect(GameConstants.SFX_CLEAR_LINE);
            }
            showCombo(comboCount);
            showScoreBonus(bonus);
        } else {
            if (comboCount > 0) {
                clearCombo();
            }
            comboCount = 0;
            if (audioManager != null) {
                audioManager.playSoundEffect(GameConstants.SFX_BLOCK_FALL);
            }
        }
    }

    public void resetCombo() {
        if (comboCount > 0) {
            clearCombo();
        }
        comboCount = 0;
    }
}