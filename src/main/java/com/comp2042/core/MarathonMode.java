package com.comp2042.core;

import com.comp2042.config.GameModeConfig;

public class MarathonMode {

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

    public int getTargetLines() {
        return targetLines;
    }

    public int getStartDifficulty() {
        return startDifficulty;
    }

    public int getCurrentSpeedMs(int linesCleared) {
        return GameModeConfig.calculateMarathonSpeed(startDifficulty, linesCleared);
    }

    public boolean isWon(int linesCleared) {
        return linesCleared >= targetLines;
    }
}