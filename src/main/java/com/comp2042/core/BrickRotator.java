package com.comp2042.core;

import com.comp2042.core.bricks.Brick;

import java.util.List;

/**
 * Manages brick rotation state and provides access to current and next rotation shapes.
 */
public class BrickRotator {

    private Brick brick;
    private List<int[][]> shapeMatrices;
    private int currentShape = 0;

    /**
     * Gets the next rotation shape for the current brick.
     * 
     * @return NextShapeInfo containing the next shape and its rotation index
     * @throws IllegalStateException if no brick is set
     */
    public NextShapeInfo getNextShape(){
        brickNotSet();
        int nextShape =(currentShape + 1) % shapeMatrices.size();
        return new NextShapeInfo(shapeMatrices.get(nextShape),nextShape);
    }

    /**
     * Gets the current rotation shape of the brick.
     * 
     * @return 2D array representing the current shape
     * @throws IllegalStateException if no brick is set
     */
    public int[][] getCurrentShape(){
        brickNotSet();
        return shapeMatrices.get(currentShape);
    }

    /**
     * Sets the current rotation state of the brick.
     * 
     * @param currentShape the rotation index to set
     * @throws IllegalStateException if no brick is set
     */
    public void setCurrentShape(int currentShape){
        brickNotSet();
        this.currentShape = currentShape;
    }

    /**
     * Sets the brick to rotate. Resets rotation to initial state.
     * 
     * @param brick the brick to set, or null to clear
     */
    public void setBrick(Brick brick){
        if (brick == null) {
            this.brick = null;
            this.shapeMatrices = null;
            currentShape = 0;
            return;
        }
        this.brick = brick;
        this.shapeMatrices = this.brick.getShapeMatrix();
        currentShape = 0;
    }

    /**
     * Gets the current brick instance.
     * 
     * @return the current brick, or null if none set
     */
    public Brick getBrick(){
        return brick;
    }

    /**
     * Validates that a brick is set before operations.
     * 
     * @throws IllegalStateException if no brick is set
     */
    private void brickNotSet(){
        if(brick == null || shapeMatrices == null || shapeMatrices.isEmpty()){
            throw new IllegalStateException("Brick is not set.");
        }
    }
}
