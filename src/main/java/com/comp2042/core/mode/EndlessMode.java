package com.comp2042.core.mode;

import com.comp2042.config.GameModeConfig;

/**
 * Endless game mode with constant speed based on difficulty level (0-10).
 * Play indefinitely (999 lines).
 */
public class EndlessMode implements GameModeStrategy {

    private static final int TARGET_LINES = 999;
    private final int difficulty;
    private final int speedMs;

    /**
     * Creates an endless mode with specified difficulty.
     * 
     * @param difficulty difficulty level (0-10)
     * @throws IllegalArgumentException if difficulty is out of range
     */
    public EndlessMode(int difficulty) {
        if (difficulty < 0 || difficulty > 10) {
            throw new IllegalArgumentException("Difficulty must be between 0 and 10");
        }
        this.difficulty = difficulty;
        this.speedMs = GameModeConfig.calculateEndlessSpeed(difficulty);
    }

    @Override
    public int getCurrentSpeedMs(int linesCleared) {
        return speedMs;
    }

    /**
     * Gets the constant speed for this mode.
     * 
     * @return speed in milliseconds
     */
    public int getSpeedMs() {
        return speedMs;
    }

    /**
     * Gets the difficulty level.
     * 
     * @return difficulty level (0-10)
     */
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
        return difficulty;
    }
}

