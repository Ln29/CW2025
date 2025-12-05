package com.comp2042.core.mode;

import com.comp2042.config.GameModeConfig;

import java.util.Random;

/**
 * Survival game mode with garbage row spawning and increasing speed.
 * Player must clear target lines while garbage rows spawn periodically.
 */
public class SurvivalMode implements GameModeStrategy {

    private final GameModeConfig.SurvivalDifficulty difficulty;
    private final Random random;
    private static final int GARBAGE_BLOCK_VALUE = 8;
    private static final int ROWS_PER_SPAWN = 2;

    /**
     * Creates a survival mode with specified difficulty.
     * 
     * @param difficulty survival difficulty level
     */
    public SurvivalMode(GameModeConfig.SurvivalDifficulty difficulty) {
        this.difficulty = difficulty;
        this.random = new Random();
    }

    /**
     * Gets the survival difficulty level.
     * 
     * @return survival difficulty
     */
    public GameModeConfig.SurvivalDifficulty getDifficulty() {
        return difficulty;
    }

    @Override
    public int getTargetLines() {
        return difficulty.getTargetLines();
    }

    /**
     * Gets the starting speed for this difficulty.
     * 
     * @return starting speed in milliseconds
     */
    public int getStartSpeedMs() {
        return difficulty.getStartSpeedMs();
    }

    /**
     * Calculates current speed based on lines cleared.
     * Speed increases every 10 lines cleared.
     * 
     * @param linesCleared number of lines cleared
     * @return speed in milliseconds (minimum 80ms)
     */
    @Override
    public int getCurrentSpeedMs(int linesCleared) {
        int baseSpeed = difficulty.getStartSpeedMs();
        int speedIncreaseFactor = linesCleared / 10;
        double multiplier = Math.pow(0.9, speedIncreaseFactor);
        return Math.max((int) (baseSpeed * multiplier), 80);
    }

    /**
     * Gets the spawn interval in seconds (fixed based on difficulty).
     * 
     * @return spawn interval in seconds
     */
    public int getSpawnIntervalSeconds() {
        return difficulty.getSpawnIntervalSeconds();
    }

    /**
     * Generates garbage rows with random gaps based on difficulty.
     * 
     * @param numRows number of rows to generate
     * @param boardWidth width of the board
     * @return 2D array of garbage rows
     */
    public int[][] generateGarbageRows(int numRows, int boardWidth) {
        int[][] rows = new int[numRows][boardWidth];

        for (int row = 0; row < numRows; row++) {
            rows[row] = generateGarbageRow(boardWidth);
        }

        return rows;
    }

    /**
     * Generates a single garbage row with random gaps based on difficulty weights.
     * 
     * @param boardWidth width of the board
     * @return array representing a garbage row
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
     * Selects gap count based on difficulty weights.
     * 
     * @return number of gaps (1, 2, or 3)
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

    /**
     * Checks if win condition is met based on lines cleared.
     * 
     * @param linesCleared number of lines cleared
     * @return true if target lines reached, false otherwise
     */
    @Override
    public boolean isWon(int linesCleared) {
        return linesCleared >= difficulty.getTargetLines();
    }

    /**
     * Gets the current level (survival mode doesn't have levels).
     * 
     * @param linesCleared number of lines cleared
     * @return always 0 for survival mode
     */
    @Override
    public int getCurrentLevel(int linesCleared) {
        return 0;
    }

    /**
     * Gets the garbage block value used in the board matrix.
     * 
     * @return garbage block value (8)
     */
    public int getGarbageBlockValue() {
        return GARBAGE_BLOCK_VALUE;
    }

    /**
     * Gets the number of garbage rows spawned per spawn event.
     * 
     * @return rows per spawn (2)
     */
    public int getRowsPerSpawn() {
        return ROWS_PER_SPAWN;
    }
}

