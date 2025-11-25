package com.comp2042.core.mode;

import com.comp2042.config.GameModeConfig;

public class EndlessMode implements GameModeStrategy {

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

    @Override
    public int getCurrentSpeedMs(int linesCleared) {
        return speedMs; // Speed is constant for endless mode
    }

    public int getSpeedMs() {
        return speedMs;
    }

    public int getDifficulty() {
        return difficulty;
    }

    @Override
    public int getTargetLines() {
        return TARGET_LINES;
    }

    @Override
    public boolean isWon(int linesCleared) {
        return linesCleared >= TARGET_LINES;
    }

    @Override
    public int getCurrentLevel(int linesCleared) {
        return difficulty; // Level equals difficulty in endless mode
    }
}

