package com.comp2042.core.bricks;

import java.util.List;

/**
 * Interface for Tetris brick pieces.
 * Defines the contract for accessing brick shape matrices in all rotation states.
 */
public interface Brick {

    /**
     * Gets the list of shape matrices representing all rotation states of the brick.
     * 
     * @return list of 2D arrays, each representing one rotation state
     */
    List<int[][]> getShapeMatrix();
}
