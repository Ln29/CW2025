package com.comp2042.controller;

import com.comp2042.config.GameModeConfig;
import com.comp2042.core.Board;
import com.comp2042.core.EndlessMode;
import com.comp2042.core.GameMode;
import com.comp2042.core.GarbageMode;
import com.comp2042.core.MarathonMode;
import javafx.animation.Timeline;
import javafx.util.Duration;

import java.util.function.Consumer;

public class GameModeController {

    private final GameModeConfig config;
    private final Board board;
    private EndlessMode endlessMode;
    private MarathonMode marathonMode;
    private GarbageMode garbageMode;

    private Timeline garbageSpawnTimeline;
    private boolean timersShouldBeRunning = false;

    private int linesCleared = 0;
    private Consumer<Integer> onSpeedChange;
    private Runnable onGameOver;
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
            case GARBAGE:
                garbageMode = new GarbageMode(config.getGarbageDifficulty());
                break;
        }
    }

    public void initTimers(Consumer<Integer> onSpeedChange, Runnable onGameOver, Runnable onGameWin, Runnable onGarbageSpawn) {
        this.onSpeedChange = onSpeedChange;
        this.onGameOver = onGameOver;
        this.onGameWin = onGameWin;
        this.onGarbageSpawn = onGarbageSpawn;

        GameMode mode = config.getCurrentMode();

        notifySpeedChange();

        if (mode == GameMode.GARBAGE) {
            initGarbageSpawnTimer();
        }
    }

    private void initGarbageSpawnTimer() {
        if (garbageMode == null) {
            return;
        }

        int spawnInterval = garbageMode.getSpawnIntervalSeconds(linesCleared) * 1000;
        if (garbageSpawnTimeline != null) {
            garbageSpawnTimeline.stop();
        }

        garbageSpawnTimeline = new Timeline(new javafx.animation.KeyFrame(
                Duration.millis(spawnInterval),
                ae -> spawnGarbageRows()
        ));
        garbageSpawnTimeline.setCycleCount(Timeline.INDEFINITE);
    }

    private void notifySpeedChange() {
        if (onSpeedChange != null) {
            onSpeedChange.accept(getCurrentSpeedMs());
        }
    }

    private void updateGarbageSpawnInterval() {
        if (garbageMode == null) {
            return;
        }

        int newInterval = garbageMode.getSpawnIntervalSeconds(linesCleared) * 1000;

        boolean shouldResume = timersShouldBeRunning;

        if (garbageSpawnTimeline != null) {
            garbageSpawnTimeline.stop();
        }

        garbageSpawnTimeline = new Timeline(new javafx.animation.KeyFrame(
                Duration.millis(newInterval),
                ae -> spawnGarbageRows()
        ));
        garbageSpawnTimeline.setCycleCount(Timeline.INDEFINITE);

        if (shouldResume) {
            garbageSpawnTimeline.play();
        }
    }

    private void spawnGarbageRows() {
        if (garbageMode == null || board == null || onGarbageSpawn == null) {
            return;
        }

        int[][] garbageRows = garbageMode.generateGarbageRows(
                garbageMode.getRowsPerSpawn(),
                board.getBoardMatrix()[0].length
        );

        board.addGarbageRowsFromBottom(garbageRows);

        if (onGarbageSpawn != null) {
            onGarbageSpawn.run();
        }
    }

    public void onLinesCleared(int lines) {
        linesCleared += lines;

        GameMode mode = config.getCurrentMode();

        switch (mode) {
            case MARATHON:
                notifySpeedChange();
                checkWinCondition();
                break;
            case GARBAGE:
                notifySpeedChange();
                updateGarbageSpawnInterval();
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
            case GARBAGE:
                if (garbageMode != null) {
                    won = garbageMode.isWon(linesCleared);
                }
                break;
        }

        if (won && onGameWin != null) {
            onGameWin.run();
        }
    }

    public int getCurrentSpeedMs() {
        GameMode mode = config.getCurrentMode();

        switch (mode) {
            case ENDLESS:
                return endlessMode != null ? endlessMode.getSpeedMs() : 500;
            case MARATHON:
                return marathonMode != null ? marathonMode.getCurrentSpeedMs(linesCleared) : 500;
            case GARBAGE:
                return garbageMode != null ? garbageMode.getCurrentSpeedMs(linesCleared) : 500;
            default:
                return 500;
        }
    }

    public void startTimers() {
        timersShouldBeRunning = true;
        if (garbageSpawnTimeline != null) {
            garbageSpawnTimeline.play();
        }
    }

    public void stopTimers() {
        timersShouldBeRunning = false;
        if (garbageSpawnTimeline != null) {
            garbageSpawnTimeline.stop();
        }
    }

    public void pauseTimers() {
        timersShouldBeRunning = false;
        if (garbageSpawnTimeline != null) {
            garbageSpawnTimeline.stop();
        }
    }

    public void resumeTimers() {
        timersShouldBeRunning = true;
        if (garbageSpawnTimeline != null) {
            garbageSpawnTimeline.play();
        }
    }

    public void reset() {
        linesCleared = 0;
        timersShouldBeRunning = false;
        stopTimers();
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
     * - Garbage: Level = 0
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
                    int baseLevel = marathonMode.getStartDifficulty();
                    int levelIncrease = linesCleared / 10;
                    return baseLevel + levelIncrease;
                }
                return 0;
            case GARBAGE:
                return 0;
            default:
                return 0;
        }
    }

    /**
     * Get the target lines for the current game mode
     * - Endless: Returns 999
     * - Marathon: Returns target lines (50, 100, 200, or 500)
     * - Garbage: Returns target lines from difficulty (50, 80, or 100)
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
            case GARBAGE:
                if (garbageMode != null) {
                    return garbageMode.getTargetLines();
                }
                return 0;
            default:
                return 0;
        }
    }
}