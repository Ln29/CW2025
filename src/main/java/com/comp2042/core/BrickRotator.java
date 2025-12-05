package com.comp2042.core;

import com.comp2042.core.bricks.Brick;

import java.util.List;

public class BrickRotator {

    private Brick brick;
    private List<int[][]> shapeMatrices;
    private int currentShape = 0;

    public NextShapeInfo getNextShape(){
        brickNotSet();
        int nextShape =(currentShape + 1) % shapeMatrices.size();
        return new NextShapeInfo(shapeMatrices.get(nextShape),nextShape);
    }

    public int[][] getCurrentShape(){
        brickNotSet();
        return shapeMatrices.get(currentShape);
    }

    public void setCurrentShape(int currentShape){
        brickNotSet();
        this.currentShape = currentShape;
    }

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

    public Brick getBrick(){
        return brick;
    }

    private void brickNotSet(){
        if(brick == null || shapeMatrices == null || shapeMatrices.isEmpty()){
            throw new IllegalStateException("Brick is not set.");
        }
    }
}
