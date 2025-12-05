package com.comp2042.core.bricks;

import com.comp2042.core.MatrixOperations;

import java.util.ArrayList;
import java.util.List;

/**
 * Factory for creating Tetris brick instances.
 * All seven Tetris pieces (I, J, L, O, S, T, Z) are defined as nested classes.
 */
public final class BrickFactory {

    private BrickFactory() {}

    /**
     * Enumeration of all Tetris brick types.
     */
    public enum BrickType {
        I, J, L, O, S, T, Z
    }

    /**
     * Creates a brick instance of the specified type.
     *
     * @param type The type of brick to create
     * @return A new brick instance of the specified type
     */
    public static Brick create(BrickType type) {
        return switch (type) {
            case I -> new IBrick();
            case J -> new JBrick();
            case L -> new LBrick();
            case O -> new OBrick();
            case S -> new SBrick();
            case T -> new TBrick();
            case Z -> new ZBrick();
        };
    }

    /**
     * Creates a list containing one instance of each brick type.
     * Used for bag generation in SevenBagBrickGenerator.
     *
     * @return A list containing all 7 brick types
     */
    public static List<Brick> createAll() {
        return List.of(
                new IBrick(),
                new JBrick(),
                new LBrick(),
                new OBrick(),
                new SBrick(),
                new TBrick(),
                new ZBrick()
        );
    }

    // Brick Implementations
    /**
     * I-piece (straight line) - 2 rotation states
     */
    private static final class IBrick implements Brick {
        private final List<int[][]> brickMatrix = new ArrayList<>();

        IBrick() {
            brickMatrix.add(new int[][]{
                    {0, 0, 0, 0},
                    {1, 1, 1, 1},
                    {0, 0, 0, 0},
                    {0, 0, 0, 0}
            });
            brickMatrix.add(new int[][]{
                    {0, 1, 0, 0},
                    {0, 1, 0, 0},
                    {0, 1, 0, 0},
                    {0, 1, 0, 0}
            });
        }

        @Override
        public List<int[][]> getShapeMatrix() {
            return MatrixOperations.deepCopyList(brickMatrix);
        }
    }

    /**
     * J-piece - 4 rotation states
     */
    private static final class JBrick implements Brick {
        private final List<int[][]> brickMatrix = new ArrayList<>();

        JBrick() {
            brickMatrix.add(new int[][]{
                    {0, 0, 0, 0},
                    {2, 2, 2, 0},
                    {0, 0, 2, 0},
                    {0, 0, 0, 0}
            });
            brickMatrix.add(new int[][]{
                    {0, 0, 0, 0},
                    {0, 2, 2, 0},
                    {0, 2, 0, 0},
                    {0, 2, 0, 0}
            });
            brickMatrix.add(new int[][]{
                    {0, 0, 0, 0},
                    {0, 2, 0, 0},
                    {0, 2, 2, 2},
                    {0, 0, 0, 0}
            });
            brickMatrix.add(new int[][]{
                    {0, 0, 2, 0},
                    {0, 0, 2, 0},
                    {0, 2, 2, 0},
                    {0, 0, 0, 0}
            });
        }

        @Override
        public List<int[][]> getShapeMatrix() {
            return MatrixOperations.deepCopyList(brickMatrix);
        }
    }

    /**
     * L-piece - 4 rotation states
     */
    private static final class LBrick implements Brick {
        private final List<int[][]> brickMatrix = new ArrayList<>();

        LBrick() {
            brickMatrix.add(new int[][]{
                    {0, 0, 0, 0},
                    {0, 3, 3, 3},
                    {0, 3, 0, 0},
                    {0, 0, 0, 0}
            });
            brickMatrix.add(new int[][]{
                    {0, 0, 0, 0},
                    {0, 3, 3, 0},
                    {0, 0, 3, 0},
                    {0, 0, 3, 0}
            });
            brickMatrix.add(new int[][]{
                    {0, 0, 0, 0},
                    {0, 0, 3, 0},
                    {3, 3, 3, 0},
                    {0, 0, 0, 0}
            });
            brickMatrix.add(new int[][]{
                    {0, 3, 0, 0},
                    {0, 3, 0, 0},
                    {0, 3, 3, 0},
                    {0, 0, 0, 0}
            });
        }

        @Override
        public List<int[][]> getShapeMatrix() {
            return MatrixOperations.deepCopyList(brickMatrix);
        }
    }

    /**
     * O-piece (square) - 1 rotation state (no rotation)
     */
    private static final class OBrick implements Brick {
        private final List<int[][]> brickMatrix = new ArrayList<>();

        OBrick() {
            brickMatrix.add(new int[][]{
                    {0, 0, 0, 0},
                    {0, 4, 4, 0},
                    {0, 4, 4, 0},
                    {0, 0, 0, 0}
            });
        }

        @Override
        public List<int[][]> getShapeMatrix() {
            return MatrixOperations.deepCopyList(brickMatrix);
        }
    }

    /**
     * S-piece - 2 rotation states
     */
    private static final class SBrick implements Brick {
        private final List<int[][]> brickMatrix = new ArrayList<>();

        SBrick() {
            brickMatrix.add(new int[][]{
                    {0, 0, 0, 0},
                    {0, 5, 5, 0},
                    {5, 5, 0, 0},
                    {0, 0, 0, 0}
            });
            brickMatrix.add(new int[][]{
                    {5, 0, 0, 0},
                    {5, 5, 0, 0},
                    {0, 5, 0, 0},
                    {0, 0, 0, 0}
            });
        }

        @Override
        public List<int[][]> getShapeMatrix() {
            return MatrixOperations.deepCopyList(brickMatrix);
        }
    }

    /**
     * T-piece - 4 rotation states
     */
    private static final class TBrick implements Brick {
        private final List<int[][]> brickMatrix = new ArrayList<>();

        TBrick() {
            brickMatrix.add(new int[][]{
                    {0, 0, 0, 0},
                    {6, 6, 6, 0},
                    {0, 6, 0, 0},
                    {0, 0, 0, 0}
            });
            brickMatrix.add(new int[][]{
                    {0, 6, 0, 0},
                    {0, 6, 6, 0},
                    {0, 6, 0, 0},
                    {0, 0, 0, 0}
            });
            brickMatrix.add(new int[][]{
                    {0, 6, 0, 0},
                    {6, 6, 6, 0},
                    {0, 0, 0, 0},
                    {0, 0, 0, 0}
            });
            brickMatrix.add(new int[][]{
                    {0, 6, 0, 0},
                    {6, 6, 0, 0},
                    {0, 6, 0, 0},
                    {0, 0, 0, 0}
            });
        }

        @Override
        public List<int[][]> getShapeMatrix() {
            return MatrixOperations.deepCopyList(brickMatrix);
        }
    }

    /**
     * Z-piece - 2 rotation states
     */
    private static final class ZBrick implements Brick {
        private final List<int[][]> brickMatrix = new ArrayList<>();

        ZBrick() {
            brickMatrix.add(new int[][]{
                    {0, 0, 0, 0},
                    {7, 7, 0, 0},
                    {0, 7, 7, 0},
                    {0, 0, 0, 0}
            });
            brickMatrix.add(new int[][]{
                    {0, 7, 0, 0},
                    {7, 7, 0, 0},
                    {7, 0, 0, 0},
                    {0, 0, 0, 0}
            });
        }

        @Override
        public List<int[][]> getShapeMatrix() {
            return MatrixOperations.deepCopyList(brickMatrix);
        }
    }
}

