package com.comp2042.controller;

import com.comp2042.config.GameConstants;
import javafx.animation.Timeline;
import javafx.beans.property.BooleanProperty;

/**
 * Manages game timers for piece movement, lock delay checks, and game time tracking.
 * Handles starting, stopping, pausing, and resuming all game timers.
 */
public class GameLifecycle {

    private final AudioManager audioManager;
    private Timeline timeLine;
    private Timeline lockDelayCheckTimeline;
    private Timeline gameTimeTimeline;

    /**
     * Checks if any timers have been initialized.
     * 
     * @return true if at least one timer exists, false otherwise
     */
    public boolean hasTimers() {
        return timeLine != null || lockDelayCheckTimeline != null || gameTimeTimeline != null;
    }

    /**
     * Creates a game lifecycle manager.
     * 
     * @param audioManager audio manager for controlling music during pause/resume
     */
    public GameLifecycle(AudioManager audioManager) {
        this.audioManager = audioManager;
    }

    /**
     * Initializes all game timers with specified callbacks and speed.
     * 
     * @param onMoveDownTick callback for piece movement tick
     * @param onLockDelayCheck callback for lock delay check tick
     * @param onGameSecondTick callback for game time tick (every second)
     * @param initialSpeedMs initial movement speed in milliseconds
     */
    public void initTimers(Runnable onMoveDownTick, Runnable onLockDelayCheck, Runnable onGameSecondTick, int initialSpeedMs) {
        timeLine = new Timeline(new javafx.animation.KeyFrame(
                javafx.util.Duration.millis(initialSpeedMs),
                ae -> { if (onMoveDownTick != null) onMoveDownTick.run(); }
        ));
        timeLine.setCycleCount(Timeline.INDEFINITE);

        lockDelayCheckTimeline = new Timeline(new javafx.animation.KeyFrame(
                javafx.util.Duration.millis(GameConstants.LOCK_CHECK_MS),
                ae -> { if (onLockDelayCheck != null) onLockDelayCheck.run(); }
        ));
        lockDelayCheckTimeline.setCycleCount(Timeline.INDEFINITE);

        gameTimeTimeline = new Timeline(new javafx.animation.KeyFrame(
                javafx.util.Duration.millis(1000),
                ae -> { if (onGameSecondTick != null) onGameSecondTick.run(); }
        ));
        gameTimeTimeline.setCycleCount(Timeline.INDEFINITE);
    }

    /**
     * Starts all game timers.
     */
    public void startTimers() {
        controlTimers(true);
    }

    /**
     * Stops all game timers.
     */
    public void stopTimers() {
        controlTimers(false);
    }

    /**
     * Pauses all timers and music.
     */
    public void pauseTimers() {
        controlTimers(false);
        if (audioManager != null) {
            audioManager.pauseAllMusic();
    }
    }

    /**
     * Resumes all timers and music.
     */
    public void resumeTimers() {
        controlTimers(true);
        if (audioManager != null) {
            audioManager.resumeAllMusic();
        }
    }

    /**
     * Handles game over - stops timers, sets game over flag, and plays lose sound.
     * 
     * @param isGameOver property to set to true when game is over
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
     * Controls all timers (start or stop).
     * 
     * @param play true to start timers, false to stop
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

    /**
     * Updates the movement timer speed and callback.
     * 
     * @param speedMs new speed in milliseconds
     * @param onMoveDownTick new callback for movement tick
     */
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


