package com.comp2042.core.mode;

/**
 * Common interface for all game mode strategies.
 * Defines the contract that all game modes must implement.
 */
public interface GameModeStrategy {
    
    /**
     * Get the current game speed in milliseconds based on lines cleared.
     * @param linesCleared The number of lines cleared so far
     * @return Speed in milliseconds (lower = faster)
     */
    int getCurrentSpeedMs(int linesCleared);
    
    /**
     * Get the target number of lines to clear for this mode.
     * @return Target lines
     */
    int getTargetLines();
    
    /**
     * Check if the player has won based on lines cleared.
     * @param linesCleared The number of lines cleared so far
     * @return true if the win condition met
     */
    boolean isWon(int linesCleared);
    
    /**
     * Get the current level based on lines cleared.
     * @param linesCleared The number of lines cleared so far
     * @return Current level (0-10 typically)
     */
    int getCurrentLevel(int linesCleared);
}

