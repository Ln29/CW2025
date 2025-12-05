package com.comp2042.core.mode;

import com.comp2042.config.GameModeConfig;

/**
 * Marathon game mode with increasing speed and target lines (50, 100, 200, or 500).
 * Speed increases every 10 lines cleared.
 */
public class MarathonMode implements GameModeStrategy {

    private final int targetLines;
    private final int startDifficulty;

    /**
     * Creates a marathon mode with specified target lines and starting difficulty.
     * 
     * @param targetLines target lines to clear (50, 100, 200, or 500)
     * @param startDifficulty starting difficulty level (0-10)
     * @throws IllegalArgumentException if parameters are invalid
     */
    public MarathonMode(int targetLines, int startDifficulty) {
        if (targetLines != 50 && targetLines != 100 && targetLines != 200 && targetLines != 500) {
            throw new IllegalArgumentException("Target lines must be 50, 100, 200, or 500");
        }
        if (startDifficulty < 0 || startDifficulty > 10) {
            throw new IllegalArgumentException("Difficulty must be between 0 and 10");
        }
        this.targetLines = targetLines;
        this.startDifficulty = startDifficulty;
    }

    /**
     * Gets the target number of lines to clear.
     * 
     * @return target lines
     */
    @Override
    public int getTargetLines() {
        return targetLines;
    }

    /**
     * Gets the starting difficulty level.
     * 
     * @return starting difficulty (0-10)
     */
    public int getStartDifficulty() {
        return startDifficulty;
    }

    @Override
    public int getCurrentSpeedMs(int linesCleared) {
        return GameModeConfig.calculateMarathonSpeed(startDifficulty, linesCleared);
    }

    @Override
    public boolean isWon(int linesCleared) {
        return linesCleared >= targetLines;
    }

    /**
     * Gets the current level based on lines cleared.
     * Level increases every 10 lines, capped at 10.
     * 
     * @param linesCleared number of lines cleared
     * @return current level (0-10)
     */
    @Override
    public int getCurrentLevel(int linesCleared) {
        int levelIncrease = linesCleared / 10;
        return Math.min(startDifficulty + levelIncrease, 10);
    }
}

