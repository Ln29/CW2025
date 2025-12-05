package com.comp2042.core;

/**
 * Represents a ghost brick - a preview of where the current piece will land.
 * The ghost brick has the same shape and color as the original piece.
 */
public final class GhostBrick {
    
    private final int[][] brickData;
    private final int xPosition;
    private final int yPosition;
    
    /**
     * Creates a ghost brick at the specified position.
     * 
     * @param brickData ghost brick shape matrix
     * @param xPosition ghost brick X position
     * @param yPosition ghost brick Y position
     */
    public GhostBrick(int[][] brickData, int xPosition, int yPosition) {
        this.brickData = MatrixOperations.copy(brickData);
        this.xPosition = xPosition;
        this.yPosition = yPosition;
    }
    
    public int[][] getBrickData() {
        return MatrixOperations.copy(brickData);
    }
    
    public int getxPosition() {
        return xPosition;
    }
    
    public int getyPosition() {
        return yPosition;
    }
}

