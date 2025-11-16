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
        boolean wasPlaying = garbageSpawnTimeline != null &&
                garbageSpawnTimeline.getStatus() == javafx.animation.Animation.Status.RUNNING;

        if (garbageSpawnTimeline != null) {
            garbageSpawnTimeline.stop();
        }

        garbageSpawnTimeline = new Timeline(new javafx.animation.KeyFrame(
                Duration.millis(newInterval),
                ae -> spawnGarbageRows()
        ));
        garbageSpawnTimeline.setCycleCount(Timeline.INDEFINITE);

        if (wasPlaying) {
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

        boolean gameOver = board.addGarbageRowsFromBottom(garbageRows);

        if (onGarbageSpawn != null) {
            onGarbageSpawn.run();
        }

        if (gameOver && onGameOver != null) {
            onGameOver.run();
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
        if (garbageSpawnTimeline != null) {
            garbageSpawnTimeline.play();
        }
    }

    public void stopTimers() {
        if (garbageSpawnTimeline != null) {
            garbageSpawnTimeline.stop();
        }
    }

    public void pauseTimers() {
        stopTimers();
    }

    public void resumeTimers() {
        startTimers();
    }

    public void reset() {
        linesCleared = 0;
        stopTimers();
        initializeMode();
    }

    public int getLinesCleared() {
        return linesCleared;
    }

    public GameMode getCurrentMode() {
        return config.getCurrentMode();
    }

    public int getCurrentLevel() {
        GameMode mode = config.getCurrentMode();

        switch (mode) {
            case ENDLESS:
                if (endlessMode != null) {
                    return endlessMode.getDifficulty(); // Display as 0-10
                }
                return 0;
            case MARATHON:
                if (marathonMode != null) {
                    int baseLevel = marathonMode.getStartDifficulty();
                    int levelIncrease = linesCleared / 10;
                    return baseLevel + levelIncrease;
                }
                return 0;
            case GARBAGE: //not 0-10 difficulty
                return 0;
            default:
                return 0;
        }
    }
}
