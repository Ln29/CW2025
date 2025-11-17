package com.comp2042.core;

import com.comp2042.config.GameModeConfig;

import java.util.Random;

public class GarbageMode {

    private final GameModeConfig.GarbageDifficulty difficulty;
    private final Random random;
    private static final int GARBAGE_BLOCK_VALUE = 8; // Gray
    private static final int ROWS_PER_SPAWN = 2;

    public GarbageMode(GameModeConfig.GarbageDifficulty difficulty) {
        this.difficulty = difficulty;
        this.random = new Random();
    }

    public GameModeConfig.GarbageDifficulty getDifficulty() {
        return difficulty;
    }

    public int getTargetLines() {
        return difficulty.getTargetLines();
    }

    public int getStartSpeedMs() {
        return difficulty.getStartSpeedMs();
    }

    public int getCurrentSpeedMs(int linesCleared) {
        int baseSpeed = difficulty.getStartSpeedMs();
        int speedIncreaseFactor = linesCleared / 10;
        double multiplier = Math.pow(0.9, speedIncreaseFactor);
        return Math.max((int) (baseSpeed * multiplier), 80); // Minimum 80ms cap
    }

    public int getSpawnIntervalSeconds() {
        return difficulty.getSpawnIntervalSeconds();
    }

    /**
     * Generate garbage rows with random gaps
     * @param numRows Number of rows to generate (typically 2)
     * @param boardWidth Width of the board
     * @return Array of garbage rows (each row is an array of block values)
     */
    public int[][] generateGarbageRows(int numRows, int boardWidth) {
        int[][] rows = new int[numRows][boardWidth];

        for (int row = 0; row < numRows; row++) {
            rows[row] = generateGarbageRow(boardWidth);
        }

        return rows;
    }

    /**
     * Generate a single garbage row with random gaps based on difficulty weights
     */
    private int[] generateGarbageRow(int boardWidth) {
        int[] row = new int[boardWidth];

        int numGaps = selectGapCount();

        for (int i = 0; i < boardWidth; i++) {
            row[i] = GARBAGE_BLOCK_VALUE;
        }

        // random gaps
        for (int i = 0; i < numGaps; i++) {
            int gapPosition = random.nextInt(boardWidth);
            while (row[gapPosition] == 0 && numGaps < boardWidth) {
                gapPosition = random.nextInt(boardWidth);
            }
            row[gapPosition] = 0;
        }

        return row;
    }

    /**
     * Select gap count based on difficulty weights
     * Returns 1, 2, or 3 gaps
     */
    private int selectGapCount() {
        double[] weights = difficulty.getGapWeights();
        double randomValue = random.nextDouble();

        double cumulative = 0.0;
        for (int i = 0; i < weights.length; i++) {
            cumulative += weights[i];
            if (randomValue < cumulative) {
                return i + 1;
            }
        }

        return 2;
    }

    public boolean isWon(int linesCleared) {
        return linesCleared >= difficulty.getTargetLines();
    }

    public boolean shouldEnd(int linesCleared) {
        return isWon(linesCleared);
    }

    public int getGarbageBlockValue() {
        return GARBAGE_BLOCK_VALUE;
    }

    public int getRowsPerSpawn() {
        return ROWS_PER_SPAWN;
    }
}