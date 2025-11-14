package com.comp2042;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public class MatrixOperations {

    // we don't want to instantiate this utility class
    private MatrixOperations() {
    }

    // check if the brick intersects with the board or goes out of bounds
    public static boolean intersect(final int[][] matrix, final int[][] brick, int x, int y) {
        // make sure the parameters are not null
        Objects.requireNonNull(matrix, "matrix cannot be null");
        Objects.requireNonNull(brick, "brick cannot be null");

        // loop through each cell of the brick
        for (int row = 0; row < brick.length; row++) {
            int[] brickRow = Objects.requireNonNull(brick[row], "brick row cannot be null");
            for (int col = 0; col < brickRow.length; col++) {
                int cell = brickRow[col];

                // skip empty cells
                if (cell == 0) {
                    continue;
                }

                // calculate where this cell would be on the board
                int targetX = x + col;
                int targetY = y + row;

                // check if it hits something or goes out of bounds
                if (isOutOfBound(matrix, targetX, targetY) || matrix[targetY][targetX] != 0) {
                    return true;
                }
            }
        }
        return false;
    }

    // check if a position is outside the board
    private static boolean isOutOfBound(int[][] matrix, int targetX, int targetY) {
        return targetX < 0
                || targetY < 0
                || targetY >= matrix.length
                || targetX >= matrix[targetY].length;
    }

    // collision with wall, not with blocks
    public static boolean isWallCollisionOnly(final int[][] matrix, final int[][] brick, int x, int y) {
        Objects.requireNonNull(matrix, "matrix cannot be null");
        Objects.requireNonNull(brick, "brick cannot be null");

        boolean hasWallCollision = false;
        boolean hasBlockCollision = false;

        // loop through each cell of the brick
        for (int row = 0; row < brick.length; row++) {
            int[] brickRow = Objects.requireNonNull(brick[row], "brick row cannot be null");
            for (int col = 0; col < brickRow.length; col++) {
                int cell = brickRow[col];

                if (cell == 0) {
                    continue;
                }

                int targetX = x + col;
                int targetY = y + row;

                if (isOutOfBound(matrix, targetX, targetY)) {
                    hasWallCollision = true;
                } else if (matrix[targetY][targetX] != 0) {
                    hasBlockCollision = true;
                }
            }
        }

        return hasWallCollision && !hasBlockCollision;
    }

    // make a copy of the matrix
    public static int[][] copy(int[][] original) {
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
        Objects.requireNonNull(brick, "brick cannot be null");

        // make a copy first so we don't change the original
        int[][] boardCopy = copy(filledFields);

        // go through each cell of the brick
        for (int row = 0; row < brick.length; row++) {
            int[] brickRow = Objects.requireNonNull(brick[row], "brick row cannot be null");
            for (int col = 0; col < brickRow.length; col++) {
                int cell = brickRow[col];

                // skip empty cells
                if (cell == 0) {
                    continue;
                }

                // calculate the target position
                int targetX = x + col;
                int targetY = y + row;

                // place the brick cell on the board if it's in bounds
                if (!isOutOfBound(boardCopy, targetX, targetY)) {
                    boardCopy[targetY][targetX] = cell;
                }
            }
        }
        return boardCopy;
    }

    // check for complete rows and remove them
    public static ClearRow checkRemoving(final int[][] matrix) {
        // make sure matrix is not null and not empty
        Objects.requireNonNull(matrix, "matrix cannot be null");
        if (matrix.length == 0) {
            throw new IllegalArgumentException("matrix must have at least one row");
        }

        int[] firstRow = Objects.requireNonNull(matrix[0], "matrix row cannot be null");
        int[][] newMatrix = new int[matrix.length][firstRow.length];
        Deque<int[]> remainingRows = new ArrayDeque<>();
        List<Integer> clearedRowIndices = new ArrayList<>();

        // check each row to see if it's complete
        for (int i = 0; i < matrix.length; i++) {
            int[] sourceRow = Objects.requireNonNull(matrix[i], "matrix row cannot be null");
            int[] tmpRow = new int[sourceRow.length];
            boolean isRowComplete = true;

            // check if all cells in this row are filled
            for (int j = 0; j < sourceRow.length; j++) {
                int cell = sourceRow[j];
                if (cell == 0) {
                    isRowComplete = false;
                }
                tmpRow[j] = cell;
            }

            // if the row is complete, mark it for clearing
            if (isRowComplete) {
                clearedRowIndices.add(i);
            } else {
                remainingRows.add(tmpRow);
            }
        }

        // fill the new matrix from bottom to top
        for (int i = matrix.length - 1; i >= 0; i--) {
            int[] row = remainingRows.pollLast();
            if (row != null) {
                newMatrix[i] = row;
            } else {
                break;
            }
        }

        // calculate the score bonus
        int linesCleared = clearedRowIndices.size();
        int scoreBonus = 50 * linesCleared * linesCleared;
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