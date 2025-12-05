package com.comp2042.input;

import com.comp2042.core.DownData;
import com.comp2042.core.ViewData;

/**
 * Interface for handling game input events.
 * Implemented by GameController to process player actions.
 */
public interface InputEventListener {

    /**
     * Handles downward movement event.
     * 
     * @param event move event
     * @return down data with clear row info and view data
     */
    DownData onDownEvent(MoveEvent event);

    /**
     * Handles left movement event.
     * 
     * @param event move event
     * @return updated view data
     */
    ViewData onLeftEvent(MoveEvent event);

    /**
     * Handles right movement event.
     * 
     * @param event move event
     * @return updated view data
     */
    ViewData onRightEvent(MoveEvent event);

    /**
     * Handles rotation event.
     * 
     * @param event move event
     * @return updated view data
     */
    ViewData onRotateEvent(MoveEvent event);

    /**
     * Handles hard drop event.
     * 
     * @return down data with clear row info and view data
     */
    DownData onHardDropEvent();

    /**
     * Creates and starts a new game.
     */
    void createNewGame();

}
