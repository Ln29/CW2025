package com.comp2042.core;

/**
 * Information about the next rotation shape for a brick.
 * Contains the shape matrix and rotation position index.
 */
public final class NextShapeInfo {

    private final int[][] shape;
    private final int position;

    /**
     * Creates next shape information.
     * 
     * @param shape shape matrix for the next rotation
     * @param position rotation position index
     */
    public NextShapeInfo(final int[][] shape, final int position) {
        this.shape = shape;
        this.position = position;
    }

    public int[][] getShape() {
        return MatrixOperations.copy(shape);
    }

    public int getPosition() {
        return position;
    }
}
