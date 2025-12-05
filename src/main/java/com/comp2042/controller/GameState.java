package com.comp2042.controller;

import com.comp2042.config.GameModeConfig;
import com.comp2042.core.mode.GameMode;
import java.util.HashMap;
import java.util.Map;

public class GameState {
    private int totalLinesCleared = 0;
    private long gameStartTime = 0L;
    private long elapsedSeconds = 0L;
    
    // High scores separated by mode and variant
    private final Map<String, Integer> highScores = new HashMap<>();

    public int getTotalLinesCleared() {
        return totalLinesCleared;
    }

    public void addClearedLines(int lines) {
        if (lines > 0) {
            this.totalLinesCleared += lines;
        }
    }

    public void resetLines() {
        this.totalLinesCleared = 0;
    }

    /**
     * Gets the high score for the specified game mode and configuration.
     * For ENDLESS: uses mode only
     * For MARATHON: uses mode + target lines
     * For SURVIVAL: uses mode + difficulty
     */
    public int getHighScore(GameMode mode, GameModeConfig config) {
        String key = getHighScoreKey(mode, config);
        return highScores.getOrDefault(key, 0);
    }
    
    /**
     * Sets the high score for the specified game mode and configuration.
     * Only updates if the new score is higher than the current high score.
     */
    public void setHighScore(int score, GameMode mode, GameModeConfig config) {
        String key = getHighScoreKey(mode, config);
        int currentHigh = highScores.getOrDefault(key, 0);
        highScores.put(key, Math.max(currentHigh, score));
    }
    
    /**
     * Convenience method to get high score using GameModeController.
     * Uses default ENDLESS mode config if GameModeController is null.
     */
    public int getHighScore(com.comp2042.controller.GameModeController gameModeController) {
        if (gameModeController == null) {
            // Use default ENDLESS mode config
            GameModeConfig defaultConfig = new GameModeConfig();
            return getHighScore(GameMode.ENDLESS, defaultConfig);
        }
        return getHighScore(gameModeController.getCurrentMode(), gameModeController.getConfig());
    }
    
    /**
     * Convenience method to set high score using GameModeController.
     * Uses default ENDLESS mode config if GameModeController is null.
     */
    public void setHighScore(int score, com.comp2042.controller.GameModeController gameModeController) {
        if (gameModeController == null) {
            // Use default ENDLESS mode config
            GameModeConfig defaultConfig = new GameModeConfig();
            setHighScore(score, GameMode.ENDLESS, defaultConfig);
            return;
        }
        setHighScore(score, gameModeController.getCurrentMode(), gameModeController.getConfig());
    }
    
    /**
     * Generates a unique key for storing high scores based on mode and configuration.
     */
    private String getHighScoreKey(GameMode mode, GameModeConfig config) {
        switch (mode) {
            case ENDLESS:
                return "ENDLESS";
            case MARATHON:
                return "MARATHON_" + config.getMarathonTargetLines();
            case SURVIVAL:
                return "SURVIVAL_" + config.getSurvivalDifficulty().name();
            default:
                return mode.name();
        }
    }

    public long getGameStartTime() {
        return gameStartTime;
    }

    public void setGameStartTimeNow() {
        this.gameStartTime = System.currentTimeMillis();
    }
    
    public long getElapsedSeconds() {
        return elapsedSeconds;
    }

    public void incrementElapsedSeconds() {
        this.elapsedSeconds++;
    }

    public void resetElapsedTime() {
        this.elapsedSeconds = 0L;
    }

    /**
     * Starts a new game by resetting the game start time and elapsed time.
     * Combines the functionality previously in StatsUpdater.startTimer().
     */
    public void startGame() {
        setGameStartTimeNow();
        resetElapsedTime();
    }
}


