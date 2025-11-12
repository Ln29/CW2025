package com.comp2042;

import com.comp2042.logic.bricks.Brick;

import java.util.List;
import java.util.Objects;

public class BrickRotator {

    private Brick brick;
    private List<int[][]> shapeMatrices;
    private int currentShape = 0;

    public NextShapeInfo getNextShape(){
        BrickNotSet();
        int nextShape =(currentShape + 1) % shapeMatrices.size();
        return new NextShapeInfo(shapeMatrices.get(nextShape),nextShape);
    }

    public int[][] getCurrentShape(){
        BrickNotSet();
        return shapeMatrices.get(currentShape);
    }

    public void setCurrentShape(int currentShape){
        BrickNotSet();
        this.currentShape = currentShape;
    }

    public void setBrick(Brick brick){
        this.brick = Objects.requireNonNull(brick, "brick cannot be null");
        this.shapeMatrices = this.brick.getShapeMatrix();
        currentShape = 0;
    }

    private void BrickNotSet(){
        if(brick == null || shapeMatrices == null || shapeMatrices.isEmpty()){
            throw new IllegalStateException("Brick is not set.");
        }
    }


}
