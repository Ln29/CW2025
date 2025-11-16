package com.comp2042.controller;

import com.comp2042.audio.AudioManager;
import com.comp2042.config.GameConstants;
import javafx.animation.Timeline;
import javafx.beans.property.BooleanProperty;

public class GameLifecycle {

    private final AudioManager audioManager;
    private Timeline timeLine;
    private Timeline lockDelayCheckTimeline;
    private Timeline gameTimeTimeline;

    public boolean hasTimers() {
        return timeLine != null || lockDelayCheckTimeline != null || gameTimeTimeline != null;
    }

    public GameLifecycle(AudioManager audioManager) {
        this.audioManager = audioManager;
    }

    public void initTimers(Runnable onMoveDownTick, Runnable onLockDelayCheck, Runnable onGameSecondTick) {
        // Main move down tick
        timeLine = new Timeline(new javafx.animation.KeyFrame(
                javafx.util.Duration.millis(GameConstants.GAME_TICK_MS),
                ae -> { if (onMoveDownTick != null) onMoveDownTick.run(); }
        ));
        timeLine.setCycleCount(Timeline.INDEFINITE);

        // Lock delay check tick
        lockDelayCheckTimeline = new Timeline(new javafx.animation.KeyFrame(
                javafx.util.Duration.millis(GameConstants.LOCK_CHECK_MS),
                ae -> { if (onLockDelayCheck != null) onLockDelayCheck.run(); }
        ));
        lockDelayCheckTimeline.setCycleCount(Timeline.INDEFINITE);

        // Game seconds timer
        gameTimeTimeline = new Timeline(new javafx.animation.KeyFrame(
                javafx.util.Duration.millis(1000),
                ae -> { if (onGameSecondTick != null) onGameSecondTick.run(); }
        ));
        gameTimeTimeline.setCycleCount(Timeline.INDEFINITE);
    }

    public void startTimers() {
        if (timeLine != null) timeLine.play();
        if (lockDelayCheckTimeline != null) lockDelayCheckTimeline.play();
        if (gameTimeTimeline != null) gameTimeTimeline.play();
    }

    public void stopTimers() {
        if (timeLine != null) timeLine.stop();
        if (lockDelayCheckTimeline != null) lockDelayCheckTimeline.stop();
        if (gameTimeTimeline != null) gameTimeTimeline.stop();
    }

    public void pauseTimers() {
        if (timeLine != null) timeLine.stop();
        if (lockDelayCheckTimeline != null) lockDelayCheckTimeline.stop();
        if (gameTimeTimeline != null) gameTimeTimeline.stop();
        if (audioManager != null) audioManager.pauseAllMusic();
    }

    public void resumeTimers() {
        if (timeLine != null) timeLine.play();
        if (lockDelayCheckTimeline != null) lockDelayCheckTimeline.play();
        if (gameTimeTimeline != null) gameTimeTimeline.play();
        if (audioManager != null) audioManager.resumeAllMusic();
    }

    public void pause(Timeline timeLine, Timeline lockDelay, Timeline gameTimer, BooleanProperty isPause) {
        if (timeLine != null) timeLine.stop();
        if (lockDelay != null) lockDelay.stop();
        if (gameTimer != null) gameTimer.stop();
        if (isPause != null) isPause.setValue(Boolean.TRUE);
        if (audioManager != null) audioManager.pauseAllMusic();
    }

    public void resume(Timeline timeLine, Timeline lockDelay, Timeline gameTimer, BooleanProperty isPause) {
        if (timeLine != null) timeLine.play();
        if (lockDelay != null) lockDelay.play();
        if (gameTimer != null) gameTimer.play();
        if (isPause != null) isPause.setValue(Boolean.FALSE);
        if (audioManager != null) audioManager.resumeAllMusic();
    }

    public void stop(Timeline timeLine, Timeline lockDelay, Timeline gameTimer) {
        if (timeLine != null) timeLine.stop();
        if (lockDelay != null) lockDelay.stop();
        if (gameTimer != null) gameTimer.stop();
    }

    public void start(Timeline timeLine, Timeline lockDelay, Timeline gameTimer, BooleanProperty isPause, BooleanProperty isGameOver) {
        if (timeLine != null) timeLine.play();
        if (lockDelay != null) lockDelay.play();
        if (gameTimer != null) gameTimer.play();
        if (isPause != null) isPause.setValue(Boolean.FALSE);
        if (isGameOver != null) isGameOver.setValue(Boolean.FALSE);
    }

    public void gameOver(Timeline timeLine, Timeline lockDelay, Timeline gameTimer, BooleanProperty isGameOver) {
        if (timeLine != null) timeLine.stop();
        if (lockDelay != null) lockDelay.stop();
        if (gameTimer != null) gameTimer.stop();
        if (isGameOver != null) isGameOver.setValue(Boolean.TRUE);
        if (audioManager != null) {
            audioManager.stopAllMusic();
            audioManager.playSoundEffect(GameConstants.SFX_LOSE);
        }
    }

    public void gameOver(BooleanProperty isGameOver) {
        stopTimers();
        if (isGameOver != null) isGameOver.setValue(Boolean.TRUE);
        if (audioManager != null) {
            audioManager.stopAllMusic();
            audioManager.playSoundEffect(GameConstants.SFX_LOSE);
        }
    }
}


