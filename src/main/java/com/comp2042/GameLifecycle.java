package com.comp2042;

import javafx.animation.Timeline;
import javafx.beans.property.BooleanProperty;

public class GameLifecycle {

    private final AudioManager audioManager;

    public GameLifecycle(AudioManager audioManager) {
        this.audioManager = audioManager;
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
            audioManager.playSoundEffect("lose.wav");
        }
    }
}