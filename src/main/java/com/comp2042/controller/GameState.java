package com.comp2042.controller;

import com.comp2042.config.GameModeConfig;
import com.comp2042.core.mode.GameMode;
import java.util.HashMap;
import java.util.Map;

/**
 * Manages game state including lines cleared, elapsed time, and mode-specific high scores.
 */
public class GameState {
    private int totalLinesCleared = 0;
    private long gameStartTime = 0L;
    private long elapsedSeconds = 0L;
    
    private final Map<String, Integer> highScores = new HashMap<>();

    /**
     * Gets the total lines cleared across all games.
     * 
     * @return total lines cleared
     */
    public int getTotalLinesCleared() {
        return totalLinesCleared;
    }

    /**
     * Adds cleared lines to the total count.
     * 
     * @param lines number of lines cleared
     */
    public void addClearedLines(int lines) {
        if (lines > 0) {
            this.totalLinesCleared += lines;
        }
    }

    /**
     * Resets the total lines cleared count.
     */
    public void resetLines() {
        this.totalLinesCleared = 0;
    }

    /**
     * Gets the high score for the specified game mode and configuration.
     * 
     * @param mode game mode
     * @param config game mode configuration
     * @return high score for the mode/variant
     */
    public int getHighScore(GameMode mode, GameModeConfig config) {
        String key = getHighScoreKey(mode, config);
        return highScores.getOrDefault(key, 0);
    }
    
    /**
     * Sets the high score for the specified game mode and configuration.
     * Only updates if the new score is higher than the current high score.
     * 
     * @param score new score to check
     * @param mode game mode
     * @param config game mode configuration
     */
    public void setHighScore(int score, GameMode mode, GameModeConfig config) {
        String key = getHighScoreKey(mode, config);
        int currentHigh = highScores.getOrDefault(key, 0);
        highScores.put(key, Math.max(currentHigh, score));
    }
    
    /**
     * Gets high score using GameModeController.
     * 
     * @param gameModeController game mode controller, or null for default ENDLESS
     * @return high score for the current mode
     */
    public int getHighScore(com.comp2042.controller.GameModeController gameModeController) {
        if (gameModeController == null) {
            GameModeConfig defaultConfig = new GameModeConfig();
            return getHighScore(GameMode.ENDLESS, defaultConfig);
        }
        return getHighScore(gameModeController.getCurrentMode(), gameModeController.getConfig());
    }
    
    /**
     * Sets high score using GameModeController.
     * 
     * @param score new score to check
     * @param gameModeController game mode controller, or null for default ENDLESS
     */
    public void setHighScore(int score, com.comp2042.controller.GameModeController gameModeController) {
        if (gameModeController == null) {
            GameModeConfig defaultConfig = new GameModeConfig();
            setHighScore(score, GameMode.ENDLESS, defaultConfig);
            return;
        }
        setHighScore(score, gameModeController.getCurrentMode(), gameModeController.getConfig());
    }
    
    /**
     * Generates a unique key for storing high scores based on mode and configuration.
     * 
     * @param mode game mode
     * @param config game mode configuration
     * @return unique key string
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

    /**
     * Gets the game start time in milliseconds.
     * 
     * @return start time timestamp
     */
    public long getGameStartTime() {
        return gameStartTime;
    }

    /**
     * Sets the game start time to the current system time.
     */
    public void setGameStartTimeNow() {
        this.gameStartTime = System.currentTimeMillis();
    }
    
    /**
     * Gets the elapsed game time in seconds.
     * 
     * @return elapsed seconds
     */
    public long getElapsedSeconds() {
        return elapsedSeconds;
    }

    /**
     * Increments the elapsed time by one second.
     */
    public void incrementElapsedSeconds() {
        this.elapsedSeconds++;
    }

    /**
     * Resets the elapsed time to zero.
     */
    public void resetElapsedTime() {
        this.elapsedSeconds = 0L;
    }

    /**
     * Starts a new game by resetting start time and elapsed time.
     */
    public void startGame() {
        setGameStartTimeNow();
        resetElapsedTime();
    }
}


