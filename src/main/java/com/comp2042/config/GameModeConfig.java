package com.comp2042.config;

import com.comp2042.core.mode.GameMode;

/**
 * Configuration for game modes including difficulty, target lines, and speed calculations.
 */
public class GameModeConfig {

    private GameMode currentMode;
    private int difficulty; 
    private int marathonTargetLines;
    private SurvivalDifficulty survivalDifficulty;

    /**
     * Survival mode difficulty levels with target lines, gap weights, speed, and spawn intervals.
     */
    public enum SurvivalDifficulty {
        SIMPLE(50, new double[]{0.2, 0.4, 0.4}, 700, 30),
        MODERATE(50, new double[]{0.3, 0.4, 0.3}, 600, 20),
        DIFFICULT(50, new double[]{0.5, 0.35, 0.15}, 500, 10);

        private final int targetLines;
        private final double[] gapWeights;
        private final int startSpeedMs;
        private final int spawnIntervalSeconds;

        SurvivalDifficulty(int targetLines, double[] gapWeights, int startSpeedMs, int spawnIntervalSeconds) {
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
        this.survivalDifficulty = SurvivalDifficulty.SIMPLE;
    }

    /**
     * Gets the current game mode.
     * 
     * @return current game mode
     */
    public GameMode getCurrentMode() {
        return currentMode;
    }

    /**
     * Sets the current game mode.
     * 
     * @param mode game mode to set
     */
    public void setCurrentMode(GameMode mode) {
        this.currentMode = mode;
    }

    /**
     * Gets the difficulty level (0-10).
     * 
     * @return difficulty level
     */
    public int getDifficulty() {
        return difficulty;
    }

    /**
     * Sets the difficulty level (0-10).
     * 
     * @param difficulty difficulty level
     * @throws IllegalArgumentException if difficulty is out of range
     */
    public void setDifficulty(int difficulty) {
        if (difficulty < 0 || difficulty > 10) {
            throw new IllegalArgumentException("Difficulty must be between 0 and 10");
        }
        this.difficulty = difficulty;
    }

    /**
     * Gets the marathon mode target lines.
     * 
     * @return target lines (50, 100, 200, or 500)
     */
    public int getMarathonTargetLines() {
        return marathonTargetLines;
    }

    /**
     * Sets the marathon mode target lines.
     * 
     * @param targetLines target lines (must be 50, 100, 200, or 500)
     * @throws IllegalArgumentException if target lines is invalid
     */
    public void setMarathonTargetLines(int targetLines) {
        if (targetLines != 50 && targetLines != 100 && targetLines != 200 && targetLines != 500) {
            throw new IllegalArgumentException("Marathon target lines must be 50, 100, 200, or 500");
        }
        this.marathonTargetLines = targetLines;
    }

    /**
     * Gets the survival mode difficulty.
     * 
     * @return survival difficulty
     */
    public SurvivalDifficulty getSurvivalDifficulty() {
        return survivalDifficulty;
    }

    /**
     * Sets the survival mode difficulty.
     * 
     * @param difficulty survival difficulty
     */
    public void setSurvivalDifficulty(SurvivalDifficulty difficulty) {
        this.survivalDifficulty = difficulty;
    }

    /**
     * Calculates speed for Endless mode based on difficulty (0-10).
     * 
     * @param difficulty difficulty level (0-10)
     * @return speed in milliseconds
     * @throws IllegalArgumentException if difficulty is out of range
     */
    public static int calculateEndlessSpeed(int difficulty) {
        if (difficulty < 0 || difficulty > 10) {
            throw new IllegalArgumentException("Difficulty must be between 0 and 10");
        }
        // Linear interpolation: 1000ms at 0, 500ms at 5, 100ms at 10
        if (difficulty <= 5) {
            return 1000 - (difficulty * 100); // 1000, 900, 800, 700, 600, 500
        } else {
            return 500 - ((difficulty - 5) * 80); // 500, 420, 340, 260, 180, 100
        }
    }

    /**
     * Calculates speed for Marathon mode based on starting difficulty and lines cleared.
     * Speed increases every 10 lines.
     * 
     * @param startDifficulty starting difficulty (0-10)
     * @param linesCleared number of lines cleared
     * @return speed in milliseconds (minimum 80ms)
     */
    public static int calculateMarathonSpeed(int startDifficulty, int linesCleared) {
        int baseSpeed = calculateEndlessSpeed(startDifficulty);
        int speedIncreaseFactor = linesCleared / 10;
        double multiplier = Math.pow(0.9, speedIncreaseFactor);
        int calculatedSpeed = (int) (baseSpeed * multiplier);
        return Math.max(calculatedSpeed, 80);
    }
}