package com.comp2042.core;

import com.comp2042.core.bricks.Brick;
import java.util.List;

/**
 * Interface defining the core game board operations for Tetris.
 * Handles piece movement, rotation, locking, row clearing, and game state management.
 */
public interface Board {

    /**
     * Moves the current brick down by one cell.
     * 
     * @return true if movement was successful, false if blocked
     */
    boolean moveBrickDown();

    /**
     * Moves the current brick left by one cell.
     * 
     * @return true if movement was successful, false if blocked
     */
    boolean moveBrickLeft();

    /**
     * Moves the current brick right by one cell.
     * 
     * @return true if movement was successful, false if blocked
     */
    boolean moveBrickRight();

    /**
     * Rotates the current brick counter-clockwise.
     * 
     * @return true if rotation was successful, false if blocked
     */
    boolean rotateLeftBrick();

    /**
     * Creates and spawns a new brick at the top of the board.
     * 
     * @return true if game over (spawn blocked), false otherwise
     */
    boolean createNewBrick();

    /**
     * Gets the current board state matrix.
     * 
     * @return 2D array representing the board (0 = empty, non-zero = filled)
     */
    int[][] getBoardMatrix();

    /**
     * Gets the current view data including active brick and next brick.
     * 
     * @return ViewData containing brick shapes and positions
     */
    ViewData getViewData();

    /**
     * Merges the current brick into the board background.
     */
    void mergeBrickToBackground();

    /**
     * Clears complete rows and returns the result.
     * 
     * @return ClearRow containing number of lines cleared and updated matrix
     */
    ClearRow clearRows();

    /**
     * Gets the score manager.
     * 
     * @return Score instance
     */
    Score getScore();

    /**
     * Resets the board to start a new game.
     */
    void newGame();

    /**
     * Gets the next bricks that will appear.
     * 
     * @param count number of next bricks to retrieve
     * @return list of upcoming bricks
     */
    List<Brick> getNextBricks(int count);

    /**
     * Holds the current brick and swaps with previously held brick.
     * 
     * @return true if hold was successful, false if already used
     */
    boolean holdBrick();

    /**
     * Gets the currently held brick.
     * 
     * @return held brick, or null if none
     */
    Brick getHeldBrick();

    /**
     * Resets the hold usage flag, allowing hold to be used again.
     */
    void resetHoldUsage();

    /**
     * Drops the brick instantly to the bottom.
     * 
     * @return number of cells the brick dropped
     */
    int hardDropBrick();

    /**
     * Checks if the lock delay has expired and piece should be locked.
     * 
     * @return true if piece should be locked, false otherwise
     */
    boolean shouldLockPiece();

    /**
     * Gets the ghost brick showing where the piece will land.
     * 
     * @return GhostBrick instance, or null if not applicable
     */
    GhostBrick getGhostBrick();

    /**
     * Adds garbage rows from the bottom (used in survival mode).
     * 
     * @param garbageRows 2D array of garbage rows to add
     * @return true if game over occurred, false otherwise
     */
    boolean addGarbageRowsFromBottom(int[][] garbageRows);
}
