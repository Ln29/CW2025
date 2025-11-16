package com.comp2042.core;

import com.comp2042.core.bricks.Brick;
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

    boolean holdBrick();

    Brick getHeldBrick();

    void resetHoldUsage();

    int hardDropBrick();

    boolean shouldLockPiece();

    GhostBrick getGhostBrick();

    boolean addGarbageRowsFromBottom(int[][] garbageRows);
}
