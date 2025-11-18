package com.comp2042.controller;

import com.comp2042.config.GameModeConfig;
import com.comp2042.core.Board;
import com.comp2042.core.EndlessMode;
import com.comp2042.core.GameMode;
import com.comp2042.core.SurvivalMode;
import com.comp2042.core.MarathonMode;

import java.util.function.Consumer;

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
     * Initialize timers for the current game mode
     */
    public void initTimers(Consumer<Integer> onSpeedChange, Runnable onGameOver, Runnable onGameWin, Runnable onGarbageSpawn) {
        this.onSpeedChange = onSpeedChange;
        // onGameOver is handled by GameLifecycle
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
     * Check if garbage should spawn based on elapsed game time
     * @param elapsedSeconds Current elapsed game time in seconds
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
     * Spawn garbage rows (called when timer indicates it's time)
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
     * Called when lines are cleared - updates mode-specific logic
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
                if (endlessMode != null) {
                    won = endlessMode.isWon(linesCleared);
                }
                break;
            case MARATHON:
                if (marathonMode != null) {
                    won = marathonMode.isWon(linesCleared);
                }
                break;
            case SURVIVAL:
                if (survivalMode != null) {
                    won = survivalMode.isWon(linesCleared);
                }
                break;
        }

        if (won && onGameWin != null) {
            onGameWin.run();
        }
    }

    /**
     * Get the current speed in milliseconds for the active mode
     */
    public int getCurrentSpeedMs() {
        GameMode mode = config.getCurrentMode();

        switch (mode) {
            case ENDLESS:
                return endlessMode != null ? endlessMode.getSpeedMs() : 500;
            case MARATHON:
                return marathonMode != null ? marathonMode.getCurrentSpeedMs(linesCleared) : 500;
            case SURVIVAL:
                return survivalMode != null ? survivalMode.getCurrentSpeedMs(linesCleared) : 500;
            default:
                return 500;
        }
    }

    /**
     * Start all timers for the current mode
     */
    public void startTimers() {
        // No separate timer needed - garbage spawn is checked via game timer
    }

    /**
     * Stop all timers
     */
    public void stopTimers() {
        // No separate timer to stop
    }

    /**
     * Pause all timers
     */
    public void pauseTimers() {
        // No separate timer to pause
    }

    /**
     * Resume all timers
     */
    public void resumeTimers() {
        // No separate timer to resume
    }

    /**
     * Reset the mode controller (e.g., for new game)
     */
    public void reset() {
        linesCleared = 0;
        lastGarbageSpawnTime = 0;
        initializeMode();
    }

    public int getLinesCleared() {
        return linesCleared;
    }

    public GameMode getCurrentMode() {
        return config.getCurrentMode();
    }

    /**
     * Get the current level based on the game mode
     * - Endless: Level = difficulty (0-10)
     * - Marathon: Level = startingDifficulty + (linesCleared / 10)
     * - Survival: Level = 0
     */
    public int getCurrentLevel() {
        GameMode mode = config.getCurrentMode();

        switch (mode) {
            case ENDLESS:
                if (endlessMode != null) {
                    return endlessMode.getDifficulty();
                }
                return 0;
            case MARATHON:
                if (marathonMode != null) {
                    // Level increases every 10 lines, starting from starting difficulty
                    // Level is capped at 10
                    int baseLevel = marathonMode.getStartDifficulty();
                    int levelIncrease = linesCleared / 10;
                    return Math.min(baseLevel + levelIncrease, 10);
                }
                return 0;
            case SURVIVAL:
                return 0;
            default:
                return 0;
        }
    }

    /**
     * Get the target lines for the current game mode
     * - Endless: Returns 999 (or -1 if no target)
     * - Marathon: Returns target lines (50, 100, 200, or 500)
     * - Survival: Returns target lines from difficulty (50, 80, or 100)
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
}