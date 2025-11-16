package com.comp2042.config;

import com.comp2042.core.GameMode;

public class GameModeConfig {

    private GameMode currentMode;
    private int difficulty; // Endless and Marathon(0-10)
    private int marathonTargetLines; // Marathon(5,100,200,500)
    private GarbageDifficulty garbageDifficulty; // Garbage mode

    public enum GarbageDifficulty {
        SIMPLE(50, new double[]{0.2, 0.4, 0.4}, 700, 3, 10),
        MODERATE(80, new double[]{0.3, 0.4, 0.3}, 600, 3, 10),
        DIFFICULT(100, new double[]{0.5, 0.35, 0.15}, 500, 4, 7);

        private final int targetLines;
        private final double[] gapWeights;
        private final int startSpeedMs;
        private final int stages;
        private final int finalStageSpawnInterval; // seconds

        GarbageDifficulty(int targetLines, double[] gapWeights, int startSpeedMs, int stages, int finalStageSpawnInterval) {
            this.targetLines = targetLines;
            this.gapWeights = gapWeights;
            this.startSpeedMs = startSpeedMs;
            this.stages = stages;
            this.finalStageSpawnInterval = finalStageSpawnInterval;
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

        public int getStages() {
            return stages;
        }

        public int getFinalStageSpawnInterval() {
            return finalStageSpawnInterval;
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

    /**
     * Calculate speed for Endless mode based on difficulty (0-10)
     * Level 0 = 1000ms, Level 5 = 500ms, Level 10 = 100ms
     */
    public static int calculateEndlessSpeed(int difficulty) {
        if (difficulty < 0 || difficulty > 10) {
            throw new IllegalArgumentException("Difficulty must be between 0 and 10");
        }
        //1000ms at 0, 100ms at 10
        if (difficulty <= 5) {
            return 1000 - (difficulty * 100); //1000, 900, 800, 700, 600, 500
        } else {
            return 500 - ((difficulty - 5) * 80); //500, 420, 340, 260, 180, 100
        }
    }

    /**
     * Calculate speed for Marathon mode based on starting difficulty and lines cleared
     * Speed increases every 10 lines
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