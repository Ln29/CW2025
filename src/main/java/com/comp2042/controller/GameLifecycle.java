package com.comp2042.controller;

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

    public void initTimers(Runnable onMoveDownTick, Runnable onLockDelayCheck, Runnable onGameSecondTick, int initialSpeedMs) {
        // Main move down tick
        timeLine = new Timeline(new javafx.animation.KeyFrame(
                javafx.util.Duration.millis(initialSpeedMs),
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

    /**
     * Starts all timers.
     */
    public void startTimers() {
        controlTimers(true);
    }

    /**
     * Stops all timers.
     */
    public void stopTimers() {
        controlTimers(false);
    }

    /**
     * Pauses all timers and audio.
     */
    public void pauseTimers() {
        controlTimers(false);
        if (audioManager != null) {
            audioManager.pauseAllMusic();
    }
    }

    /**
     * Resumes all timers and audio.
     */
    public void resumeTimers() {
        controlTimers(true);
        if (audioManager != null) {
            audioManager.resumeAllMusic();
        }
    }

    /**
     * Handles game over state - stops timers, sets game over flag, and plays lose sound.
     * 
     * @param isGameOver The property to set to true when game is over
     */
    public void gameOver(BooleanProperty isGameOver) {
        stopTimers();
        if (isGameOver != null) {
            isGameOver.setValue(Boolean.TRUE);
        }
        if (audioManager != null) {
            audioManager.stopAllMusic();
            audioManager.playSoundEffect(GameConstants.SFX_LOSE);
        }
    }

    /**
     * Internal helper method to control all timers (play or stop).
     * 
     * @param play If true, starts all timers; if false, stops all timers
     */
    private void controlTimers(boolean play) {
        if (timeLine != null) {
            if (play) {
                timeLine.play();
            } else {
                timeLine.stop();
            }
        }
        if (lockDelayCheckTimeline != null) {
            if (play) {
                lockDelayCheckTimeline.play();
            } else {
                lockDelayCheckTimeline.stop();
            }
        }
        if (gameTimeTimeline != null) {
            if (play) {
                gameTimeTimeline.play();
            } else {
                gameTimeTimeline.stop();
            }
        }
    }

    //Update the game tick speed with a new action
    public void updateSpeed(int speedMs, Runnable onMoveDownTick) {
        if (timeLine != null) {
            boolean wasPlaying = timeLine.getStatus() == javafx.animation.Animation.Status.RUNNING;
            timeLine.stop();
            timeLine = new Timeline(new javafx.animation.KeyFrame(
                    javafx.util.Duration.millis(speedMs),
                    ae -> { if (onMoveDownTick != null) onMoveDownTick.run(); }
            ));
            timeLine.setCycleCount(Timeline.INDEFINITE);
            if (wasPlaying) {
                timeLine.play();
            }
        }
    }
}


