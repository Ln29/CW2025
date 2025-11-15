package com.comp2042;

import java.util.Objects;

public class GameController implements InputEventListener {

    private final Board board;
    private final GuiController viewGuiController;

    public GameController(GuiController guiController) {
        this(guiController, new SimpleBoard(10, 22));
    }

    GameController(GuiController guiController, Board board) {
        // Make sure parameters are not null
        this.viewGuiController = Objects.requireNonNull(guiController, "guiController cannot be null");
        this.board = Objects.requireNonNull(board, "board cannot be null");

        // Initialize the game
        this.board.createNewBrick();
        this.viewGuiController.setEventListener(this);
        this.viewGuiController.setBoard(this.board);
        this.viewGuiController.initGameView(this.board.getBoardMatrix(), this.board.getViewData());
        this.viewGuiController.bindScore(this.board.getScore().scoreProperty());

    }

    @Override
    public DownData onDownEvent(MoveEvent event) {
        boolean canMove = board.moveBrickDown();
        ClearRow clearRow = null;

        if (!canMove) {
            if (board.shouldLockPiece()) {
                // Lock delay expired, lock the piece
                board.mergeBrickToBackground();
                clearRow = board.clearRows();

                if (clearRow.getLinesRemoved() > 0) {
                    board.getScore().add(clearRow.getScoreBonus());
                }

                if (board.createNewBrick()) {
                    viewGuiController.gameOver();
                }

                viewGuiController.refreshGameBackground(board.getBoardMatrix());
            }
        }

        return new DownData(clearRow, board.getViewData());
    }

    @Override
    public ViewData onLeftEvent(MoveEvent event) {
        board.moveBrickLeft();
        return board.getViewData();
    }

    @Override
    public ViewData onRightEvent(MoveEvent event) {
        board.moveBrickRight();
        return board.getViewData();
    }

    @Override
    public ViewData onRotateEvent(MoveEvent event) {
        board.rotateLeftBrick();
        return board.getViewData();
    }

    @Override
    public DownData onHardDropEvent() {
        board.hardDropBrick();

        board.mergeBrickToBackground();
        ClearRow clearRow = board.clearRows();

        if (clearRow.getLinesRemoved() > 0) {
            board.getScore().add(clearRow.getScoreBonus());
        }
        if (board.createNewBrick()) {
            viewGuiController.gameOver();
        }
        viewGuiController.refreshGameBackground(board.getBoardMatrix());
        return new DownData(clearRow, board.getViewData());
    }

    @Override
    public void createNewGame() {
        board.newGame();
        viewGuiController.refreshGameBackground(board.getBoardMatrix());
    }

}