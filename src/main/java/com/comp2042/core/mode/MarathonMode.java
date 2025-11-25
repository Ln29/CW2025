package com.comp2042.core.mode;

import com.comp2042.config.GameModeConfig;

public class MarathonMode implements GameModeStrategy {

    private final int targetLines;
    private final int startDifficulty;

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

    @Override
    public int getTargetLines() {
        return targetLines;
    }

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

    @Override
    public int getCurrentLevel(int linesCleared) {
        // Level increases every 10 lines, starting from starting difficulty
        // Level is capped at 10
        int levelIncrease = linesCleared / 10;
        return Math.min(startDifficulty + levelIncrease, 10);
    }
}

