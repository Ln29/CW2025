package com.comp2042;

import com.comp2042.logic.bricks.Brick;
import java.util.List;

public interface Board {

    boolean moveBrickDown();

    boolean moveBrickLeft();

    boolean moveBrickRight();

    boolean rotateLeftBrick();

    boolean createNewBrick();

    int[][] getBoardMatrix();

    ViewData getViewData();

    void mergeBrickToBackground();

    ClearRow clearRows();

    Score getScore();

    void newGame();

    List<Brick> getNextBricks(int count);

    /**
     * Holds the current brick and spawns the next one, or swaps with the held brick.
     * Can only be used once per brick drop.
     * @return true if hold was successful, false if hold was already used for this brick
     */
    boolean holdBrick();

    /**
     * Gets the currently held brick, or null if no brick is held.
     * @return the held brick or null
     */
    Brick getHeldBrick();

    /**
     * Resets the hold usage flag (called when a brick is placed/fixed).
     */
    void resetHoldUsage();

    /**
     * Hard drops the current brick to the bottom instantly.
     * @return the number of rows the brick was dropped
     */
    int hardDropBrick();
}
