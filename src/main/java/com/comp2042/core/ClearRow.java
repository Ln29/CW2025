package com.comp2042.core;

/**
 * Result of row clearing operation containing lines removed count,
 * updated board matrix, and score bonus.
 */
public final class ClearRow {

    private final int linesRemoved;
    private final int[][] newMatrix;
    private final int scoreBonus;

    /**
     * Creates a clear row result.
     * 
     * @param linesRemoved number of lines cleared
     * @param newMatrix updated board matrix after clearing
     * @param scoreBonus score points awarded
     */
    public ClearRow(int linesRemoved, int[][] newMatrix, int scoreBonus) {
        this.linesRemoved = linesRemoved;
        this.newMatrix = MatrixOperations.copy(newMatrix);
        this.scoreBonus = scoreBonus;
    }

    public int getLinesRemoved() {
        return linesRemoved;
    }

    public int[][] getNewMatrix() {
        return MatrixOperations.copy(newMatrix);
    }

    public int getScoreBonus() {
        return scoreBonus;
    }
}
