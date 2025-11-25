package com.comp2042.core.mode;

import com.comp2042.config.GameModeConfig;

import java.util.Random;

public class SurvivalMode implements GameModeStrategy {

    private final GameModeConfig.SurvivalDifficulty difficulty;
    private final Random random;
    private static final int GARBAGE_BLOCK_VALUE = 8;
    private static final int ROWS_PER_SPAWN = 2;

    public SurvivalMode(GameModeConfig.SurvivalDifficulty difficulty) {
        this.difficulty = difficulty;
        this.random = new Random();
    }

    public GameModeConfig.SurvivalDifficulty getDifficulty() {
        return difficulty;
    }

    @Override
    public int getTargetLines() {
        return difficulty.getTargetLines();
    }

    public int getStartSpeedMs() {
        return difficulty.getStartSpeedMs();
    }

    /**
     * Calculate current speed based on lines cleared
     * Speed increases every 10 lines cleared
     */
    @Override
    public int getCurrentSpeedMs(int linesCleared) {
        int baseSpeed = difficulty.getStartSpeedMs();
        int speedIncreaseFactor = linesCleared / 10;
        double multiplier = Math.pow(0.9, speedIncreaseFactor);
        return Math.max((int) (baseSpeed * multiplier), 80);
    }

    /**
     * Get the spawn interval in seconds (fixed based on difficulty)
     * SIMPLE: 15 seconds, MODERATE: 10 seconds, DIFFICULT: 7 seconds
     */
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

        // Determine number of gaps based on difficulty weights
        int numGaps = selectGapCount();

        // Fill row with garbage blocks
        for (int i = 0; i < boardWidth; i++) {
            row[i] = GARBAGE_BLOCK_VALUE;
        }

        // Place gaps randomly
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


    @Override
    public boolean isWon(int linesCleared) {
        return linesCleared >= difficulty.getTargetLines();
    }

    @Override
    public int getCurrentLevel(int linesCleared) {
        return 0; // Survival mode doesn't have levels
    }

    public int getGarbageBlockValue() {
        return GARBAGE_BLOCK_VALUE;
    }

    public int getRowsPerSpawn() {
        return ROWS_PER_SPAWN;
    }
}

