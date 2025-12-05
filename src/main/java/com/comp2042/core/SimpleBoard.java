package com.comp2042.core;

import com.comp2042.core.bricks.Brick;
import com.comp2042.core.bricks.BrickGenerator;
import com.comp2042.core.bricks.SevenBagBrickGenerator;

import java.awt.*;
import java.util.List;

/**
 * Implementation of the Board interface for Tetris game logic.
 * Manages the game board state, piece movement, rotation, locking, and row clearing.
 */
public class SimpleBoard implements Board {

    private static final long LOCK_DELAY_MS = 500;

    private final int width;
    private final int height;
    private final BrickGenerator brickGenerator;
    private final BrickRotator brickRotator;
    private int[][] currentGameMatrix;
    private Point currentOffset;
    private final Score score;
    private Brick heldBrick;
    private boolean holdUsed;
    private Long lockDelayStartTime;

    /**
     * Creates a new game board with specified dimensions.
     * 
     * @param width board width in cells
     * @param height board height in cells
     */
    public SimpleBoard(int width, int height) {
        this.width = width;
        this.height = height;
        currentGameMatrix = new int[height][width];
        brickGenerator = new SevenBagBrickGenerator();
        brickRotator = new BrickRotator();
        score = new Score();
    }

    /**
     * Moves the brick down by one cell. Starts lock delay timer if blocked.
     * 
     * @return true if movement successful, false if blocked
     */
    @Override
    public boolean moveBrickDown() {
        Point p = new Point(currentOffset);
        p.translate(0, 1);
        boolean conflict = MatrixOperations.intersect(currentGameMatrix, brickRotator.getCurrentShape(), 
                (int) p.getX(), (int) p.getY());
        if (conflict) {
            if (lockDelayStartTime == null) {
                lockDelayStartTime = System.currentTimeMillis();
            }
            return false;
        }
        currentOffset = p;
        lockDelayStartTime = null;
        return true;
    }


    /**
     * Moves the brick left by one cell.
     * 
     * @return true if movement successful, false if blocked
     */
    @Override
    public boolean moveBrickLeft() {
        return moveBrickHorizontally(-1);
    }

    /**
     * Moves the brick right by one cell.
     * 
     * @return true if movement successful, false if blocked
     */
    @Override
    public boolean moveBrickRight() {
        return moveBrickHorizontally(1);
    }

    /**
     * Moves the brick horizontally by the specified delta.
     * 
     * @param deltaX the horizontal movement amount (negative for left, positive for right)
     * @return true if the movement was successful, false if blocked
     */
    private boolean moveBrickHorizontally(int deltaX) {
        Point p = new Point(currentOffset);
        p.translate(deltaX, 0);
        boolean conflict = MatrixOperations.intersect(currentGameMatrix, brickRotator.getCurrentShape(), 
                (int) p.getX(), (int) p.getY());
        if (conflict) {
            return false;
        }
        currentOffset = p;
        resetLockDelayIfCanMoveDown();
        return true;
    }

    /**
     * Rotates the brick counter-clockwise. Attempts wall kicks if blocked by walls only.
     * 
     * @return true if rotation successful, false if blocked
     */
    @Override
    public boolean rotateLeftBrick() {
        NextShapeInfo nextShape = brickRotator.getNextShape();
        int currentX = (int) currentOffset.getX();
        int currentY = (int) currentOffset.getY();

        boolean conflict = MatrixOperations.intersect(currentGameMatrix, nextShape.getShape(), currentX, currentY);
        if (!conflict) {
            brickRotator.setCurrentShape(nextShape.getPosition());
            resetLockDelayIfCanMoveDown();
            return true;
        }

        // Try wall kick if rotation blocked by wall only
        if (MatrixOperations.isWallCollisionOnly(currentGameMatrix, nextShape.getShape(), currentX, currentY)) {
            int[] wallKickOffsets = {1, -1, 2, -2};

            for (int offset : wallKickOffsets) {
                int newX = currentX + offset;
                if (!MatrixOperations.intersect(currentGameMatrix, nextShape.getShape(), newX, currentY)) {
                    brickRotator.setCurrentShape(nextShape.getPosition());
                    currentOffset = new Point(newX, currentY);
                    resetLockDelayIfCanMoveDown();
                    return true;
                }
            }
        }

        return false;
    }

    /**
     * Checks if the lock delay has expired and the piece should be locked.
     * 
     * @return true if lock delay expired, false otherwise
     */
    public boolean shouldLockPiece() {
        if (lockDelayStartTime == null) {
            return false;
        }
        long elapsed = System.currentTimeMillis() - lockDelayStartTime;
        return elapsed >= LOCK_DELAY_MS;
    }

    /**
     * Resets the lock delay if the piece can now move down after an adjustment.
     */
    private void resetLockDelayIfCanMoveDown() {
        Point testPoint = new Point(currentOffset);
        testPoint.translate(0, 1);
        boolean canMoveDown = !MatrixOperations.intersect(currentGameMatrix, brickRotator.getCurrentShape(),
                (int) testPoint.getX(), (int) testPoint.getY());
        if (canMoveDown) {
            lockDelayStartTime = null;
        }
    }

    /**
     * Creates and spawns a new brick. Tries hidden rows first, then visible area.
     * 
     * @return true if game over (spawn blocked), false otherwise
     */
    @Override
    public boolean createNewBrick() {
        Brick currentBrick = brickGenerator.getBrick();
        brickRotator.setBrick(currentBrick);
        currentOffset = createSpawnPoint();
        lockDelayStartTime = null;

        int spawnX = (int) currentOffset.getX();
        int[][] shape = brickRotator.getCurrentShape();

        // Try to spawn in hidden rows first
        boolean canSpawnInHiddenArea = false;
        for (int y = 0; y < com.comp2042.config.GameConstants.HIDDEN_ROW_COUNT; y++) {
            if (!MatrixOperations.intersect(currentGameMatrix, shape, spawnX, y)) {
                canSpawnInHiddenArea = true;
                currentOffset = new Point(spawnX, y);
                break;
            }
        }

        if (canSpawnInHiddenArea) {
            return false;
        }

        boolean visibleAreaBlocked = false;
        for (int x = 0; x < width; x++) {
            if (currentGameMatrix[2][x] != 0) {
                visibleAreaBlocked = true;
                break;
            }
        }

        return visibleAreaBlocked;
    }

    /**
     * Gets the current board state matrix.
     * 
     * @return 2D array representing the board
     */
    @Override
    public int[][] getBoardMatrix() {
        return currentGameMatrix;
    }

    /**
     * Gets view data including current brick shape, position, and next brick.
     * 
     * @return ViewData instance
     */
    @Override
    public ViewData getViewData() {
        return new ViewData(brickRotator.getCurrentShape(), (int) currentOffset.getX(), (int) currentOffset.getY(), brickGenerator.getNextBrick().getShapeMatrix().get(0));
    }

    /**
     * Merges the current brick into the board background matrix.
     */
    @Override
    public void mergeBrickToBackground() {
        currentGameMatrix = MatrixOperations.merge(currentGameMatrix, brickRotator.getCurrentShape(), (int) currentOffset.getX(), (int) currentOffset.getY());
    }

    /**
     * Clears complete rows and updates the board matrix.
     * 
     * @return ClearRow with lines cleared count and updated matrix
     */
    @Override
    public ClearRow clearRows() {
        ClearRow clearRow = MatrixOperations.checkRemoving(currentGameMatrix);
        currentGameMatrix = clearRow.getNewMatrix();
        return clearRow;

    }

    /**
     * Gets the score manager instance.
     * 
     * @return Score instance
     */
    @Override
    public Score getScore() {
        return score;
    }

    /**
     * Resets the board, score, and spawns a new brick for a new game.
     */
    @Override
    public void newGame() {
        currentGameMatrix = new int[height][width];
        score.reset();
        heldBrick = null;
        holdUsed = false;
        lockDelayStartTime = null;
        createNewBrick();
    }

    /**
     * Holds the current brick and swaps with previously held brick.
     * Can only be used once per piece spawn.
     * 
     * @return true if hold successful, false if already used or invalid
     */
    @Override
    public boolean holdBrick() {
        if (holdUsed) {
            return false;
        }

        Brick currentBrick = brickRotator.getBrick();
        if (currentBrick == null) {
            return false;
        }

        Point spawnPoint = createSpawnPoint();
        // use held brick if available, otherwise get new one
        Brick brickToSpawn = (heldBrick == null) ? brickGenerator.getBrick() : heldBrick;
        
        brickRotator.setBrick(brickToSpawn);
        if (MatrixOperations.intersect(currentGameMatrix, brickRotator.getCurrentShape(), 
                (int) spawnPoint.getX(), (int) spawnPoint.getY())) {
            brickRotator.setBrick(currentBrick); 
            return false;
        }

        // Success: swap bricks and update state
        heldBrick = currentBrick;
        currentOffset = spawnPoint;
        holdUsed = true;
        return true;
    }

    /**
     * Gets the currently held brick.
     * 
     * @return held brick, or null if none
     */
    @Override
    public Brick getHeldBrick() {
        return heldBrick;
    }

    /**
     * Resets the hold usage flag, allowing hold to be used again.
     */
    @Override
    public void resetHoldUsage() {
        holdUsed = false;
    }

    /**
     * Gets the next bricks that will appear.
     * 
     * @param count number of next bricks to retrieve
     * @return list of upcoming bricks
     */
    @Override
    public List<Brick> getNextBricks(int count) {
        return brickGenerator.getNextBricks(count);
    }

    /**
     * Instantly drops the brick to the bottom of the board.
     * 
     * @return number of cells the brick dropped
     */
    @Override
    public int hardDropBrick() {
        int dropCount = 0;
        while (moveBrickDown()) {
            dropCount++;
        }
        return dropCount;
    }

    /**
     * Calculates and returns the ghost brick showing where the piece will land.
     * 
     * @return GhostBrick instance, or null if piece is already at bottom
     */
    @Override
    public GhostBrick getGhostBrick() {
        if (brickRotator.getCurrentShape() == null) {
            return null;
        }

        int[][] currentShape = brickRotator.getCurrentShape();
        int currentX = (int) currentOffset.getX();
        int currentY = (int) currentOffset.getY();

        int ghostY = currentY;

        while (true) {
            int testY = ghostY + 1;
            if (MatrixOperations.intersect(currentGameMatrix, currentShape, currentX, testY)) {
                break;
            }
            ghostY = testY;
        }

        if (ghostY == currentY) {
            return null;
        }

        return new GhostBrick(currentShape, currentX, ghostY);
    }

    /**
     * Adds garbage rows from the bottom, shifting existing rows up.
     * Used in survival mode.
     * 
     * @param garbageRows 2D array of garbage rows to add
     * @return true if game over occurred, false otherwise
     */
    @Override
    public boolean addGarbageRowsFromBottom(int[][] garbageRows) {
        if (garbageRows == null || garbageRows.length == 0) {
            return false;
        }

        int numRowsToAdd = garbageRows.length;
        boolean hadActiveBrick = (brickRotator.getCurrentShape() != null);
        boolean brickWasLocked = false;

        // Check if active brick will collide with incoming garbage
        if (hadActiveBrick) {
            int[][] shape = brickRotator.getCurrentShape();
            int brickY = (int) currentOffset.getY();

            int brickLowestRow = -1;
            for (int row = 0; row < shape.length; row++) {
                for (int col = 0; col < shape[row].length; col++) {
                    if (shape[row][col] != 0) {
                        int boardRow = brickY + row;
                        if (boardRow > brickLowestRow) {
                            brickLowestRow = boardRow;
                        }
                    }
                }
            }

            int garbageSpawnPosition = height - numRowsToAdd;

            if (brickLowestRow >= garbageSpawnPosition) {
                mergeBrickToBackground();
                brickRotator.setBrick(null);
                lockDelayStartTime = null;
                brickWasLocked = true;
            }
        }

        // Shift board up by numRowsToAdd
        int[][] newMatrix = new int[height][width];
        for (int i = 0; i < height; i++) {
            int sourceRow = i + numRowsToAdd;
            if (sourceRow < height) {
                System.arraycopy(currentGameMatrix[sourceRow], 0, newMatrix[i], 0, width);
            } else {
                for (int j = 0; j < width; j++) {
                    newMatrix[i][j] = 0;
                }
            }
        }

        // Add garbage rows at bottom
        for (int i = 0; i < numRowsToAdd && i < height; i++) {
            int targetRow = height - numRowsToAdd + i;
            if (targetRow >= 0 && targetRow < height) {
                int[] garbageRow = garbageRows[i];
                if (garbageRow != null && garbageRow.length == width) {
                    System.arraycopy(garbageRow, 0, newMatrix[targetRow], 0, width);
                }
            }
        }

        currentGameMatrix = newMatrix;

        // Move remaining brick up
        if (hadActiveBrick && !brickWasLocked) {
            int newBrickY = (int) currentOffset.getY() - numRowsToAdd;
            currentOffset = new Point((int) currentOffset.getX(), newBrickY);

            // Game over if brick pushed out of bounds
            if (newBrickY < 0) {
                return true;
            }
        }

        if (brickWasLocked) {
            return createNewBrick();
        }

        return false;
    }

    private Point createSpawnPoint() {
        int spawnX = (width-4) / 2;
        int spawnY = 0;
        return new Point(spawnX, spawnY);
    }
}