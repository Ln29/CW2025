package com.comp2042.config;

import com.comp2042.core.GameMode;

public class GameModeConfig {

    private GameMode currentMode;
    private int difficulty;
    private int marathonTargetLines;
    private GarbageDifficulty garbageDifficulty;

    public enum GarbageDifficulty {
        SIMPLE(50, new double[]{0.2, 0.4, 0.4}, 700, 30),
        MODERATE(50, new double[]{0.3, 0.4, 0.3}, 600, 20),
        DIFFICULT(100, new double[]{0.5, 0.35, 0.15}, 500, 10);

        private final int targetLines;
        private final double[] gapWeights;
        private final int startSpeedMs;
        private final int spawnIntervalSeconds;

        GarbageDifficulty(int targetLines, double[] gapWeights, int startSpeedMs, int spawnIntervalSeconds) {
            this.targetLines = targetLines;
            this.gapWeights = gapWeights;
            this.startSpeedMs = startSpeedMs;
            this.spawnIntervalSeconds = spawnIntervalSeconds;
        }

        public int getTargetLines() {
            return targetLines;
        }

        public double[] getGapWeights() {
            return gapWeights;
        }

        public int getStartSpeedMs() {
            return startSpeedMs;
        }

        public int getSpawnIntervalSeconds() {
            return spawnIntervalSeconds;
        }
    }

    public GameModeConfig() {
        this.currentMode = GameMode.ENDLESS;
        this.difficulty = 5;
        this.marathonTargetLines = 100;
        this.garbageDifficulty = GarbageDifficulty.SIMPLE;
    }

    public GameMode getCurrentMode() {
        return currentMode;
    }

    public void setCurrentMode(GameMode mode) {
        this.currentMode = mode;
    }

    public int getDifficulty() {
        return difficulty;
    }

    public void setDifficulty(int difficulty) {
        if (difficulty < 0 || difficulty > 10) {
            throw new IllegalArgumentException("Difficulty must be between 0 and 10");
        }
        this.difficulty = difficulty;
    }

    public int getMarathonTargetLines() {
        return marathonTargetLines;
    }

    public void setMarathonTargetLines(int targetLines) {
        if (targetLines != 50 && targetLines != 100 && targetLines != 200 && targetLines != 500) {
            throw new IllegalArgumentException("Marathon target lines must be 50, 100, 200, or 500");
        }
        this.marathonTargetLines = targetLines;
    }

    public GarbageDifficulty getGarbageDifficulty() {
        return garbageDifficulty;
    }

    public void setGarbageDifficulty(GarbageDifficulty difficulty) {
        this.garbageDifficulty = difficulty;
    }

    public static int calculateEndlessSpeed(int difficulty) {
        if (difficulty < 0 || difficulty > 10) {
            throw new IllegalArgumentException("Difficulty must be between 0 and 10");
        }
        if (difficulty <= 5) {
            return 1000 - (difficulty * 100);
        } else {
            return 500 - ((difficulty - 5) * 80);
        }
    }

    /**
     * Speed increases every 10 lines: multiply by 0.9^(lines/10)
     * Minimum speed cap: 80ms
     */
    public static int calculateMarathonSpeed(int startDifficulty, int linesCleared) {
        int baseSpeed = calculateEndlessSpeed(startDifficulty);
        int speedIncreaseFactor = linesCleared / 10;
        double multiplier = Math.pow(0.9, speedIncreaseFactor);
        int calculatedSpeed = (int) (baseSpeed * multiplier);
        return Math.max(calculatedSpeed, 80);
    }
}