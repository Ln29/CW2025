package com.comp2042;

import com.comp2042.logic.bricks.Brick;
import com.comp2042.logic.bricks.BrickGenerator;
import com.comp2042.logic.bricks.SevenBagBrickGenerator;

import java.awt.*;
import java.util.List;

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

    public SimpleBoard(int width, int height) {
        this.width = width;
        this.height = height;
        currentGameMatrix = new int[height][width];
        brickGenerator = new SevenBagBrickGenerator();
        brickRotator = new BrickRotator();
        score = new Score();
    }

    @Override
    public boolean moveBrickDown() {
        int[][] currentMatrix = MatrixOperations.copy(currentGameMatrix);
        Point p = new Point(currentOffset);
        p.translate(0, 1);
        boolean conflict = MatrixOperations.intersect(currentMatrix, brickRotator.getCurrentShape(), (int) p.getX(), (int) p.getY());
        if (conflict) {
            if (lockDelayStartTime == null) {
                lockDelayStartTime = System.currentTimeMillis();
            }
            return false;
        } else {
            currentOffset = p;
            lockDelayStartTime = null;
            return true;
        }
    }


    @Override
    public boolean moveBrickLeft() {
        int[][] currentMatrix = MatrixOperations.copy(currentGameMatrix);
        Point p = new Point(currentOffset);
        p.translate(-1, 0);
        boolean conflict = MatrixOperations.intersect(currentMatrix, brickRotator.getCurrentShape(), (int) p.getX(), (int) p.getY());
        if (conflict) {
            return false;
        } else {
            currentOffset = p;
            resetLockDelayIfCanMoveDown();
            return true;
        }
    }

    @Override
    public boolean moveBrickRight() {
        int[][] currentMatrix = MatrixOperations.copy(currentGameMatrix);
        Point p = new Point(currentOffset);
        p.translate(1, 0);
        boolean conflict = MatrixOperations.intersect(currentMatrix, brickRotator.getCurrentShape(), (int) p.getX(), (int) p.getY());
        if (conflict) {
            return false;
        } else {
            currentOffset = p;
            resetLockDelayIfCanMoveDown();
            return true;
        }
    }

    @Override
    public boolean rotateLeftBrick() {
        int[][] currentMatrix = MatrixOperations.copy(currentGameMatrix);
        NextShapeInfo nextShape = brickRotator.getNextShape();
        int currentX = (int) currentOffset.getX();
        int currentY = (int) currentOffset.getY();

        // Try normal rotation first
        boolean conflict = MatrixOperations.intersect(currentMatrix, nextShape.getShape(), currentX, currentY);
        if (!conflict) {
            brickRotator.setCurrentShape(nextShape.getPosition());
            resetLockDelayIfCanMoveDown();
            return true;
        }

        if (MatrixOperations.isWallCollisionOnly(currentMatrix, nextShape.getShape(), currentX, currentY)) {
            // Try wall kick offsets: r1, l1, r2, l2
            int[] wallKickOffsets = {1, -1, 2, -2};

            for (int offset : wallKickOffsets) {
                int newX = currentX + offset;
                if (!MatrixOperations.intersect(currentMatrix, nextShape.getShape(), newX, currentY)) {
                    brickRotator.setCurrentShape(nextShape.getPosition());
                    currentOffset = new Point(newX, currentY);
                    resetLockDelayIfCanMoveDown();
                    return true;
                }
            }
        }

        return false;
    }

    public boolean shouldLockPiece() {
        if (lockDelayStartTime == null) {
            return false;
        }
        long elapsed = System.currentTimeMillis() - lockDelayStartTime;
        return elapsed >= LOCK_DELAY_MS;
    }

    private void resetLockDelayIfCanMoveDown() {
        // Check if piece can move down from current position
        int[][] currentMatrix = MatrixOperations.copy(currentGameMatrix);
        Point testPoint = new Point(currentOffset);
        testPoint.translate(0, 1);
        boolean canMoveDown = !MatrixOperations.intersect(currentMatrix, brickRotator.getCurrentShape(),
                (int) testPoint.getX(), (int) testPoint.getY());
        if (canMoveDown) {
            // Piece can move down, reset lock delay
            lockDelayStartTime = null;
        }
    }

    @Override
    public boolean createNewBrick() {
        Brick currentBrick = brickGenerator.getBrick();
        brickRotator.setBrick(currentBrick);
        currentOffset = createSpawnPoint();
        lockDelayStartTime = null;
        return MatrixOperations.intersect(currentGameMatrix, brickRotator.getCurrentShape(), (int) currentOffset.getX(), (int) currentOffset.getY());
    }

    @Override
    public int[][] getBoardMatrix() {
        return currentGameMatrix;
    }

    @Override
    public ViewData getViewData() {
        return new ViewData(brickRotator.getCurrentShape(), (int) currentOffset.getX(), (int) currentOffset.getY(), brickGenerator.getNextBrick().getShapeMatrix().get(0));
    }

    @Override
    public void mergeBrickToBackground() {
        currentGameMatrix = MatrixOperations.merge(currentGameMatrix, brickRotator.getCurrentShape(), (int) currentOffset.getX(), (int) currentOffset.getY());
    }

    @Override
    public ClearRow clearRows() {
        ClearRow clearRow = MatrixOperations.checkRemoving(currentGameMatrix);
        currentGameMatrix = clearRow.getNewMatrix();
        return clearRow;

    }

    @Override
    public Score getScore() {
        return score;
    }


    @Override
    public void newGame() {
        currentGameMatrix = new int[height][width];
        score.reset();
        heldBrick = null;
        holdUsed = false;
        lockDelayStartTime = null;
        createNewBrick();
    }

    @Override
    public boolean holdBrick() {
        // Can only hold once per brick drop
        if (holdUsed) {
            return false;
        }

        // Get the current brick type
        Brick currentBrick = brickRotator.getBrick();
        if (currentBrick == null) {
            return false;
        }

        Point spawnPoint = createSpawnPoint();

        if (heldBrick == null) {
            Brick nextBrick = brickGenerator.getBrick();
            brickRotator.setBrick(nextBrick);
            if (MatrixOperations.intersect(currentGameMatrix, brickRotator.getCurrentShape(), (int) spawnPoint.getX(), (int) spawnPoint.getY())) {
                brickRotator.setBrick(currentBrick);
                return false;
            }
            heldBrick = currentBrick;
            currentOffset = spawnPoint;
            holdUsed = true;
            return true;
        } else {
            //swap with current
            Brick temp = heldBrick;
            brickRotator.setBrick(temp);
            if (MatrixOperations.intersect(currentGameMatrix, brickRotator.getCurrentShape(), (int) spawnPoint.getX(), (int) spawnPoint.getY())) {
                brickRotator.setBrick(currentBrick);
                return false;
            }
            heldBrick = currentBrick;
            currentOffset = spawnPoint;
            holdUsed = true;
            return true;
        }
    }

    @Override
    public Brick getHeldBrick() {
        return heldBrick;
    }

    @Override
    public void resetHoldUsage() {
        holdUsed = false;
    }

    @Override
    public List<Brick> getNextBricks(int count) {
        return brickGenerator.getNextBricks(count);
    }

    @Override
    public int hardDropBrick() {
        int dropCount = 0;
        // Keep moving the brick down until it can't move anymore
        while (moveBrickDown()) {
            dropCount++;
        }
        return dropCount;
    }

    @Override
    public GhostBrick getGhostBrick() {
        if (brickRotator.getCurrentShape() == null) {
            return null;
        }

        int[][] currentShape = brickRotator.getCurrentShape();
        int currentX = (int) currentOffset.getX();
        int currentY = (int) currentOffset.getY();
        int ghostY = currentY;
        int[][] testMatrix = MatrixOperations.copy(currentGameMatrix);

        while (true) {
            int testY = ghostY + 1;
            if (MatrixOperations.intersect(testMatrix, currentShape, currentX, testY)) {
                break;
            }
            ghostY = testY;
        }
        if (ghostY == currentY) {
            return null;
        }
        return new GhostBrick(currentShape, currentX, ghostY);
    }

    private Point createSpawnPoint() {
        int spawnX = (width-4) / 2;
        int spawnY = 0;
        return new Point(spawnX, spawnY);
    }
}
