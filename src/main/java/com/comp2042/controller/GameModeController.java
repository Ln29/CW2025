package com.comp2042.controller;

import com.comp2042.config.GameModeConfig;
import com.comp2042.core.Board;
import com.comp2042.core.mode.EndlessMode;
import com.comp2042.core.mode.GameMode;
import com.comp2042.core.mode.SurvivalMode;
import com.comp2042.core.mode.MarathonMode;

import java.util.function.Consumer;

/**
 * Controls game mode-specific logic including speed progression, win conditions and garbage row spawning.
 */
public class GameModeController {

    private final GameModeConfig config;
    private final Board board;
    private EndlessMode endlessMode;
    private MarathonMode marathonMode;
    private SurvivalMode survivalMode;

    private int linesCleared = 0;
    private long lastGarbageSpawnTime = 0;
    private Consumer<Integer> onSpeedChange;
    private Runnable onGameWin;
    private Runnable onGarbageSpawn;

    /**
     * Creates a game mode controller with specified configuration.
     * 
     * @param config game mode configuration
     * @param board game board instance
     */
    public GameModeController(GameModeConfig config, Board board) {
        this.config = config;
        this.board = board;
        initializeMode();
    }

    private void initializeMode() {
        GameMode mode = config.getCurrentMode();

        switch (mode) {
            case ENDLESS:
                endlessMode = new EndlessMode(config.getDifficulty());
                break;
            case MARATHON:
                marathonMode = new MarathonMode(config.getMarathonTargetLines(), config.getDifficulty());
                break;
            case SURVIVAL:
                survivalMode = new SurvivalMode(config.getSurvivalDifficulty());
                break;
        }
    }

    /**
     * Initializes callbacks for game mode events.
     * 
     * @param onSpeedChange callback when speed changes
     * @param onGameOver callback when game over (handled by GameLifecycle)
     * @param onGameWin callback when win condition met
     * @param onGarbageSpawn callback when garbage rows spawn
     */
    public void initTimers(Consumer<Integer> onSpeedChange, Runnable onGameOver, Runnable onGameWin, Runnable onGarbageSpawn) {
        this.onSpeedChange = onSpeedChange;
        this.onGameWin = onGameWin;
        this.onGarbageSpawn = onGarbageSpawn;
        notifySpeedChange();
    }

    private void notifySpeedChange() {
        if (onSpeedChange != null) {
            onSpeedChange.accept(getCurrentSpeedMs());
        }
    }

    /**
     * Checks if garbage rows should spawn based on elapsed time (survival mode only).
     * 
     * @param elapsedSeconds current elapsed game time in seconds
     * @return true if garbage should spawn, false otherwise
     */
    public boolean shouldSpawnGarbage(long elapsedSeconds) {
        if (survivalMode == null || config.getCurrentMode() != GameMode.SURVIVAL) {
            return false;
        }

        int spawnInterval = survivalMode.getSpawnIntervalSeconds();

        // Check if enough time has passed since last spawn
        if (elapsedSeconds >= lastGarbageSpawnTime + spawnInterval) {
            lastGarbageSpawnTime = elapsedSeconds;
            return true;
        }

        return false;
    }

    /**
     * Spawns garbage rows from the bottom (survival mode only).
     */
    public void spawnGarbageRows() {
        if (survivalMode == null || board == null || onGarbageSpawn == null) {
            return;
        }

        int[][] garbageRows = survivalMode.generateGarbageRows(
                survivalMode.getRowsPerSpawn(),
                board.getBoardMatrix()[0].length
        );

        board.addGarbageRowsFromBottom(garbageRows);

        if (onGarbageSpawn != null) {
            onGarbageSpawn.run();
        }
    }

    /**
     * Called when lines are cleared - updates mode-specific speed and win conditions.
     * 
     * @param lines number of lines cleared
     */
    public void onLinesCleared(int lines) {
        linesCleared += lines;

        GameMode mode = config.getCurrentMode();

        switch (mode) {
            case MARATHON:
                notifySpeedChange(); // Speed increases with lines
                checkWinCondition();
                break;
            case SURVIVAL:
                notifySpeedChange(); // Speed increases with lines
                checkWinCondition();
                break;
            case ENDLESS:
                checkWinCondition();
                break;
        }
    }

    private void checkWinCondition() {
        boolean won = false;

        GameMode mode = config.getCurrentMode();
        switch (mode) {
            case ENDLESS:
                won = endlessMode != null && endlessMode.isWon(linesCleared);
                break;
            case MARATHON:
                won = marathonMode != null && marathonMode.isWon(linesCleared);
                break;
            case SURVIVAL:
                won = survivalMode != null && survivalMode.isWon(linesCleared);
                break;
        }

        if (won && onGameWin != null) {
            onGameWin.run();
        }
    }

    /**
     * Gets the current movement speed in milliseconds for the active mode.
     * 
     * @return speed in milliseconds
     */
    public int getCurrentSpeedMs() {
        GameMode mode = config.getCurrentMode();

        switch (mode) {
            case ENDLESS:
                return endlessMode != null ? endlessMode.getCurrentSpeedMs(linesCleared) : 500;
            case MARATHON:
                return marathonMode != null ? marathonMode.getCurrentSpeedMs(linesCleared) : 500;
            case SURVIVAL:
                return survivalMode != null ? survivalMode.getCurrentSpeedMs(linesCleared) : 500;
            default:
                return 500;
        }
    }

    /**
     * Resets the mode controller for a new game.
     */
    public void reset() {
        linesCleared = 0;
        lastGarbageSpawnTime = 0;
        initializeMode();
    }

    /**
     * Gets the total number of lines cleared in the current game.
     * 
     * @return lines cleared count
     */
    public int getLinesCleared() {
        return linesCleared;
    }

    /**
     * Gets the current game mode.
     * 
     * @return current GameMode
     */
    public GameMode getCurrentMode() {
        return config.getCurrentMode();
    }

    /**
     * Gets the current level based on game mode and lines cleared.
     * 
     * @return current level
     */
    public int getCurrentLevel() {
        GameMode mode = config.getCurrentMode();

        switch (mode) {
            case ENDLESS:
                return endlessMode != null ? endlessMode.getCurrentLevel(linesCleared) : 0;
            case MARATHON:
                return marathonMode != null ? marathonMode.getCurrentLevel(linesCleared) : 0;
            case SURVIVAL:
                return survivalMode != null ? survivalMode.getCurrentLevel(linesCleared) : 0;
            default:
                return 0;
        }
    }

    /**
     * Gets the target lines for the current game mode.
     * 
     * @return target lines (999 for Endless, mode-specific for others)
     */
    public int getTargetLines() {
        GameMode mode = config.getCurrentMode();

        switch (mode) {
            case ENDLESS:
                return 999;
            case MARATHON:
                if (marathonMode != null) {
                    return marathonMode.getTargetLines();
                }
                return 0;
            case SURVIVAL:
                if (survivalMode != null) {
                    return survivalMode.getTargetLines();
                }
                return 0;
            default:
                return 0;
        }
    }
    
    /**
     * Gets the marathon target lines from configuration.
     * 
     * @return marathon target lines
     */
    public int getMarathonTargetLines() {
        return config.getMarathonTargetLines();
    }
    
    /**
     * Gets the survival difficulty from configuration.
     * 
     * @return survival difficulty setting
     */
    public GameModeConfig.SurvivalDifficulty getSurvivalDifficulty() {
        return config.getSurvivalDifficulty();
    }
    
    /**
     * Gets the game mode configuration.
     * 
     * @return GameModeConfig instance
     */
    public GameModeConfig getConfig() {
        return config;
    }
}