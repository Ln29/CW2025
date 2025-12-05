package com.comp2042.core.bricks;

import java.util.List;

/**
 * Interface for generating Tetris bricks.
 * Defines methods for getting current and upcoming bricks.
 */
public interface BrickGenerator {

    /**
     * Gets the next brick and removes it from the generator.
     * 
     * @return next brick to use
     */
    Brick getBrick();

    /**
     * Peeks at the next brick without removing it.
     * 
     * @return next brick that will be returned by getBrick()
     */
    Brick getNextBrick();

    /**
     * Gets the next N bricks without removing them.
     * 
     * @param count number of bricks to peek at
     * @return list of next N bricks
     */
    List<Brick> getNextBricks(int count);
}
