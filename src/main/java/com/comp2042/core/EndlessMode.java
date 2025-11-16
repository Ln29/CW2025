package com.comp2042.core;

import com.comp2042.config.GameModeConfig;

public class EndlessMode {

    private static final int TARGET_LINES = 999;
    private final int difficulty;
    private final int speedMs;

    public EndlessMode(int difficulty) {
        if (difficulty < 0 || difficulty > 10) {
            throw new IllegalArgumentException("Difficulty must be between 0 and 10");
        }
        this.difficulty = difficulty;
        this.speedMs = GameModeConfig.calculateEndlessSpeed(difficulty);
    }

    public int getSpeedMs() {
        return speedMs;
    }

    public int getDifficulty() {
        return difficulty;
    }

    public int getTargetLines() {
        return TARGET_LINES;
    }
    //999 lines
    public boolean isWon(int linesCleared) {
        return linesCleared >= TARGET_LINES;
    }

    public boolean shouldEnd(int linesCleared) {
        return isWon(linesCleared);
    }
}