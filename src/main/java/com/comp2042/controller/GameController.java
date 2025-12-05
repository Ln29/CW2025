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

/**
 * Handles game input events and coordinates between the board logic and UI.
 * Translates user input into board operations and manages piece locking.
 */
public class GameController implements InputEventListener {

    private final Board board;
    private final GuiController viewGuiController;

    /**
     * Creates a game controller with default board dimensions (10x22).
     * 
     * @param guiController the UI controller to coordinate with
     */
    public GameController(GuiController guiController) {
        this(guiController, new SimpleBoard(10, 22));
    }

    /**
     * Creates a game controller with a custom board.
     * 
     * @param guiController the UI controller to coordinate with
     * @param board the game board instance
     */
    GameController(GuiController guiController, Board board) {
        this.viewGuiController = Objects.requireNonNull(guiController, "guiController cannot be null");
        this.board = Objects.requireNonNull(board, "board cannot be null");

        // initialize the game, create the initial brick
        this.viewGuiController.setEventListener(this);
        this.viewGuiController.setBoard(this.board);

        // create the first brick so rendering has valid data
        this.board.createNewBrick();

        // initialize view with current brick data
        ViewData initialViewData = this.board.getViewData();
        this.viewGuiController.initGameView(this.board.getBoardMatrix(), initialViewData);
        this.viewGuiController.bindScore(this.board.getScore().scoreProperty());
    }

    /**
     * Handles downward movement event. Locks piece if movement blocked and lock delay expired.
     * 
     * @param event the move event
     * @return DownData with clear row info and view data
     */
    @Override
    public DownData onDownEvent(MoveEvent event) {
        boolean canMove = board.moveBrickDown();
        if (!canMove && board.shouldLockPiece()) {
            return lockPieceAndProcess();
        }
        return new DownData(null, board.getViewData());
    }

    /**
     * Handles left movement event.
     * 
     * @param event the move event
     * @return updated view data
     */
    @Override
    public ViewData onLeftEvent(MoveEvent event) {
        board.moveBrickLeft();
        return board.getViewData();
    }

    /**
     * Handles right movement event.
     * 
     * @param event the move event
     * @return updated view data
     */
    @Override
    public ViewData onRightEvent(MoveEvent event) {
        board.moveBrickRight();
        return board.getViewData();
    }

    /**
     * Handles rotation event.
     * 
     * @param event the move event
     * @return updated view data
     */
    @Override
    public ViewData onRotateEvent(MoveEvent event) {
        board.rotateLeftBrick();
        return board.getViewData();
    }

    /**
     * Handles hard drop event - instantly drops piece to bottom and locks it.
     * 
     * @return DownData with clear row info and view data
     */
    @Override
    public DownData onHardDropEvent() {
        board.hardDropBrick();
        return lockPieceAndProcess();
    }

    /**
     * Resets the board and starts a new game.
     */
    @Override
    public void createNewGame() {
        board.newGame();

        viewGuiController.refreshGameBackground(board.getBoardMatrix());
    }

    /**
     * Locks the current piece, clears rows, updates score, and spawns a new brick.
     * This method encapsulates the common logic used when a piece is locked.
     * 
     * @return DownData containing the clear row information and updated view data
     */
    private DownData lockPieceAndProcess() {
        board.mergeBrickToBackground();
        ClearRow clearRow = board.clearRows();

        if (clearRow.getLinesRemoved() > 0) {
            board.getScore().add(clearRow.getScoreBonus());
        }

        if (board.createNewBrick()) {
            viewGuiController.gameOver();
        } else {
            board.resetHoldUsage();
        }

        viewGuiController.refreshGameBackground(board.getBoardMatrix());

        return new DownData(clearRow, board.getViewData());
    }
}