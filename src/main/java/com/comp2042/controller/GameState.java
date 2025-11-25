package com.comp2042.controller;

import com.comp2042.config.GameModeConfig;
import com.comp2042.core.mode.GameMode;
import java.util.HashMap;
import java.util.Map;

public class GameState {
    private int totalLinesCleared = 0;
    private int highScore = 0; // Kept for backward compatibility, but deprecated
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
     * @deprecated Use getHighScore(GameMode, GameModeConfig) instead
     */
    @Deprecated
    public int getHighScore() {
        return highScore;
    }

    /**
     * @deprecated Use setHighScore(int, GameMode, GameModeConfig) instead
     */
    @Deprecated
    public void setHighScore(int highScore) {
        this.highScore = Math.max(this.highScore, highScore);
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
        // Also update legacy highScore for backward compatibility
        this.highScore = Math.max(this.highScore, score);
    }
    
    /**
     * Convenience method to get high score using GameModeController.
     */
    public int getHighScore(com.comp2042.controller.GameModeController gameModeController) {
        if (gameModeController == null) {
            return getHighScore(); // Fallback to legacy method
        }
        return getHighScore(gameModeController.getCurrentMode(), gameModeController.getConfig());
    }
    
    /**
     * Convenience method to set high score using GameModeController.
     */
    public void setHighScore(int score, com.comp2042.controller.GameModeController gameModeController) {
        if (gameModeController == null) {
            setHighScore(score); // Fallback to legacy method
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


