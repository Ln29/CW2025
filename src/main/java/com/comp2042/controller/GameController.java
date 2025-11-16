package com.comp2042.controller;

import com.comp2042.core.Board;
import com.comp2042.core.ClearRow;
import com.comp2042.core.DownData;
import com.comp2042.core.SimpleBoard;
import com.comp2042.core.ViewData;
import com.comp2042.input.InputEventListener;
import com.comp2042.input.MoveEvent;
import com.comp2042.ui.GuiController;
import java.util.Objects;

public class GameController implements InputEventListener {

    private final Board board;
    private final GuiController viewGuiController;

    public GameController(GuiController guiController) {
        this(guiController, new SimpleBoard(10, 22));
    }

    GameController(GuiController guiController, Board board) {
        // make sure parameters are not null
        this.viewGuiController = Objects.requireNonNull(guiController, "guiController cannot be null");
        this.board = Objects.requireNonNull(board, "board cannot be null");

        // initialize the game and create the initial brick
        this.viewGuiController.setEventListener(this);
        this.viewGuiController.setBoard(this.board);

        // create the first brick so rendering has valid data
        this.board.createNewBrick();

        // initialize view with the current brick data
        ViewData initialViewData = this.board.getViewData();
        this.viewGuiController.initGameView(this.board.getBoardMatrix(), initialViewData);
        this.viewGuiController.bindScore(this.board.getScore().scoreProperty());
    }

    @Override
    public DownData onDownEvent(MoveEvent event) {
        boolean canMove = board.moveBrickDown();
        ClearRow clearRow = null;

        if (!canMove) {
            // brick can't move down - check if lock delay has expired
            if (board.shouldLockPiece()) {
                // lock delay expired, lock the piece
                board.mergeBrickToBackground();
                clearRow = board.clearRows();

                // add score if lines were cleared
                if (clearRow.getLinesRemoved() > 0) {
                    board.getScore().add(clearRow.getScoreBonus());
                }

                // try to create new brick, check if game over
                if (board.createNewBrick()) {
                    viewGuiController.gameOver();
                } else {
                    // new brick spawned successfully; allow hold again
                    board.resetHoldUsage();
                }

                // update the display
                viewGuiController.refreshGameBackground(board.getBoardMatrix());
            }
            // if lock delay hasn't expired, don't lock yet - allow adjustments
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
        // hard drop the brick to the bottom
        board.hardDropBrick();

        // merge the brick to the background
        board.mergeBrickToBackground();
        ClearRow clearRow = board.clearRows();

        // add score if lines were cleared
        if (clearRow.getLinesRemoved() > 0) {
            board.getScore().add(clearRow.getScoreBonus());
        }

        // try to create new brick, check if game over
        if (board.createNewBrick()) {
            viewGuiController.gameOver();
        } else {
            // new brick spawned successfully; allow hold again
            board.resetHoldUsage();
        }

        // update the display
        viewGuiController.refreshGameBackground(board.getBoardMatrix());

        return new DownData(clearRow, board.getViewData());
    }

    @Override
    public void createNewGame() {
        board.newGame();

        viewGuiController.refreshGameBackground(board.getBoardMatrix());
    }
}