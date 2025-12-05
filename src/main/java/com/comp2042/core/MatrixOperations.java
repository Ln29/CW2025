package com.comp2042.core;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.function.BiConsumer;
import java.util.stream.Collectors;

public class MatrixOperations {

    // we don't want to instantiate this utility class
    private MatrixOperations() {
    }

    /**
     * Iterates through all non-empty cells of a brick and applies an action.
     * 
     * @param brick The brick shape matrix
     * @param x Base X position
     * @param y Base Y position
     * @param action Action to perform for each non-empty cell (targetX, targetY)
     */
    private static void forEachBrickCell(final int[][] brick, int x, int y, 
                                         BiConsumer<Integer, Integer> action) {
        for (int row = 0; row < brick.length; row++) {
            int[] brickRow = Objects.requireNonNull(brick[row], "brick row cannot be null");
            for (int col = 0; col < brickRow.length; col++) {
                if (brickRow[col] != 0) {
                    action.accept(x + col, y + row);
                }
            }
        }
    }

    /**
     * Functional interface for actions that need both position and cell value.
     */
    @FunctionalInterface
    private interface BrickCellAction {
        void accept(int targetX, int targetY, int cellValue);
    }

    /**
     * Iterates through all non-empty cells of a brick and applies an action with cell value.
     * 
     * @param brick The brick shape matrix
     * @param x Base X position
     * @param y Base Y position
     * @param action Action to perform for each non-empty cell (targetX, targetY, cellValue)
     */
    private static void forEachBrickCellWithValue(final int[][] brick, int x, int y,
                                                  BrickCellAction action) {
        for (int row = 0; row < brick.length; row++) {
            int[] brickRow = Objects.requireNonNull(brick[row], "brick row cannot be null");
            for (int col = 0; col < brickRow.length; col++) {
                int cell = brickRow[col];
                if (cell != 0) {
                    action.accept(x + col, y + row, cell);
                }
            }
        }
    }

    // check if the brick intersects with the board or goes out of bounds
    public static boolean intersect(final int[][] matrix, final int[][] brick, int x, int y) {
        // make sure the parameters are not null
        Objects.requireNonNull(matrix, "matrix cannot be null");
        Objects.requireNonNull(brick, "brick cannot be null");

        final boolean[] hasConflict = {false};
        forEachBrickCell(brick, x, y, (targetX, targetY) -> {
            if (isOutOfBound(matrix, targetX, targetY) || matrix[targetY][targetX] != 0) {
                hasConflict[0] = true;
            }
        });
        return hasConflict[0];
    }

    // check if a position is outside the board
    private static boolean isOutOfBound(int[][] matrix, int targetX, int targetY) {
        return targetX < 0
                || targetY < 0
                || targetY >= matrix.length
                || targetX >= matrix[targetY].length;
    }

    // check if the brick would only collide with walls (out of bounds), not with blocks
    // returns true if collision is only due to walls, false if it's due to blocks or no collision
    public static boolean isWallCollisionOnly(final int[][] matrix, final int[][] brick, int x, int y) {
        Objects.requireNonNull(matrix, "matrix cannot be null");
        Objects.requireNonNull(brick, "brick cannot be null");

        final boolean[] hasWallCollision = {false};
        final boolean[] hasBlockCollision = {false};

        forEachBrickCell(brick, x, y, (targetX, targetY) -> {
            if (isOutOfBound(matrix, targetX, targetY)) {
                hasWallCollision[0] = true;
            } else if (matrix[targetY][targetX] != 0) {
                hasBlockCollision[0] = true;
            }
        });

        // return true only if there's a wall collision but no block collision
        return hasWallCollision[0] && !hasBlockCollision[0];
    }

    // make a copy of the matrix
    public static int[][] copy(int[][] original) {
        // check if the original matrix is null
        Objects.requireNonNull(original, "original matrix cannot be null");

        int[][] result = new int[original.length][];

        // copy each row
        for (int i = 0; i < original.length; i++) {
            int[] sourceRow = Objects.requireNonNull(original[i], "matrix row cannot be null");
            int[] destinationRow = new int[sourceRow.length];
            System.arraycopy(sourceRow, 0, destinationRow, 0, sourceRow.length);
            result[i] = destinationRow;
        }
        return result;
    }

    // merge the brick into the board at a specific position
    public static int[][] merge(int[][] filledFields, int[][] brick, int x, int y) {
        // check if brick is null
        Objects.requireNonNull(brick, "brick cannot be null");

        // make a copy first so we don't change the original
        int[][] boardCopy = copy(filledFields);

        // go through each cell of the brick
        forEachBrickCellWithValue(brick, x, y, (targetX, targetY, cellValue) -> {
            if (!isOutOfBound(boardCopy, targetX, targetY)) {
                boardCopy[targetY][targetX] = cellValue;
            }
        });
        return boardCopy;
    }

    // check for complete rows and remove them
    public static ClearRow checkRemoving(final int[][] matrix) {
        Objects.requireNonNull(matrix, "matrix cannot be null");
        if (matrix.length == 0) {
            throw new IllegalArgumentException("matrix must have at least one row");
        }

        int width = Objects.requireNonNull(matrix[0], "matrix row cannot be null").length;
        List<Integer> clearedRowIndices = new ArrayList<>();

        // First pass: identify complete rows (early exit for better performance)
        for (int i = 0; i < matrix.length; i++) {
            int[] row = Objects.requireNonNull(matrix[i], "matrix row cannot be null");
            boolean isRowComplete = true;
            
            // Early exit: stop checking as soon as we find an empty cell
            for (int j = 0; j < width; j++) {
                if (row[j] == 0) {
                    isRowComplete = false;
                    break;
                }
            }
            
            if (isRowComplete) {
                clearedRowIndices.add(i);
            }
        }

        // Early return if no rows to clear
        if (clearedRowIndices.isEmpty()) {
            return new ClearRow(0, matrix, 0);
        }

        // Second pass: build new matrix by copying non-cleared rows from bottom to top
        int[][] newMatrix = new int[matrix.length][width];
        int targetRow = matrix.length - 1;
        
        for (int sourceRow = matrix.length - 1; sourceRow >= 0; sourceRow--) {
            if (!clearedRowIndices.contains(sourceRow)) {
                newMatrix[targetRow--] = matrix[sourceRow].clone(); // Faster than manual copy
            }
        }
        
        // Fill remaining rows with zeros (empty rows at top)
        while (targetRow >= 0) {
            newMatrix[targetRow--] = new int[width];
        }

        // Calculate score bonus: 100 * linesCleared * (linesCleared + 1) / 2
        int linesCleared = clearedRowIndices.size();
        int scoreBonus = 100 * linesCleared * (linesCleared + 1) / 2;
        
        return new ClearRow(linesCleared, newMatrix, scoreBonus);
    }

    // make a deep copy of a list of matrices
    public static List<int[][]> deepCopyList(List<int[][]> list) {
        // check if the list is null
        Objects.requireNonNull(list, "list cannot be null");
        return list.stream()
                .map(MatrixOperations::copy)
                .collect(Collectors.toList());
    }
}