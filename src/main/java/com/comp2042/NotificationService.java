package com.comp2042;

import javafx.scene.Group;

public class NotificationService {
    private final AudioManager audioManager;
    private final Group notificationGroup;
    private final int maxChildren;

    public NotificationService(AudioManager audioManager, Group notificationGroup, int maxChildren) {
        this.audioManager = audioManager;
        this.notificationGroup = notificationGroup;
        this.maxChildren = maxChildren;
    }

    public void showScoreBonus(int bonus) {
        Ui.run(() -> {
            if (notificationGroup == null) return;
            if (notificationGroup.getChildren().size() > maxChildren) {
                notificationGroup.getChildren().remove(0);
            }
            NotificationPanel panel = new NotificationPanel("+" + bonus);
            notificationGroup.getChildren().add(panel);
            panel.showScore(notificationGroup.getChildren());
        });
    }

    public void onLinesCleared(int linesRemoved, int bonus) {
        if (linesRemoved > 0) {
            if (audioManager != null) {
                audioManager.playSoundEffect("clearline.wav");
            }
            showScoreBonus(bonus);
        } else {
            if (audioManager != null) {
                audioManager.playSoundEffect("blockfall.wav");
            }
        }
    }
}