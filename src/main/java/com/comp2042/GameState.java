package com.comp2042;

public class GameState {
    private int totalLinesCleared = 0;
    private int highScore = 0;
    private long gameStartTime = 0L;

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
}
