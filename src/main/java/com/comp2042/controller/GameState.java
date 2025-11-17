package com.comp2042.controller;

public class GameState {
    private int totalLinesCleared = 0;
    private int highScore = 0;
    private long gameStartTime = 0L;
    private long elapsedSeconds = 0L;

    public int getTotalLinesCleared() {
        return totalLinesCleared;
    }

    public void addClearedLines(int lines) {
        if (lines > 0) {
            this.totalLinesCleared += lines;
        }
    }

    public void resetLines() {
        this.totalLinesCleared = 0;
    }

    public int getHighScore() {
        return highScore;
    }

    public void setHighScore(int highScore) {
        this.highScore = Math.max(this.highScore, highScore);
    }

    public long getGameStartTime() {
        return gameStartTime;
    }

    public void setGameStartTimeNow() {
        this.gameStartTime = System.currentTimeMillis();
    }
    public long getElapsedSeconds() {
        return elapsedSeconds;
    }

    public void incrementElapsedSeconds() {
        this.elapsedSeconds++;
    }

    public void resetElapsedTime() {
        this.elapsedSeconds = 0L;
    }
}


