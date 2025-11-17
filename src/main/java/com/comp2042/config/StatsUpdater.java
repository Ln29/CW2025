package com.comp2042.config;

import com.comp2042.controller.GameModeController;
import com.comp2042.controller.GameState;
import com.comp2042.core.Board;
import com.comp2042.ui.panels.StatsPanel;
import com.comp2042.ui.panels.StatsPanelRight;

public class StatsUpdater {

    public void updateAllStats(GameState state, Board board, StatsPanel statsPanel, StatsPanelRight statsPanelRight) {
        updateAllStats(state, board, statsPanel, statsPanelRight, null);
    }

    public void updateAllStats(GameState state, Board board, StatsPanel statsPanel, StatsPanelRight statsPanelRight, GameModeController gameModeController) {
        if (state == null) return;

        // Update high score from current score if higher
        if (board != null && board.getScore() != null && board.getScore().scoreProperty() != null) {
            int currentScore = board.getScore().scoreProperty().getValue();
            state.setHighScore(currentScore);
        }

        if (statsPanel != null) {

            if (gameModeController != null) {
                String modeName = gameModeController.getCurrentMode().name();
                modeName = modeName.substring(0, 1) + modeName.substring(1).toLowerCase();
                statsPanel.updateMode(modeName);
            } else {
                statsPanel.updateMode("Endless");
            }

            // Get level from game mode controller if available
            int level = 1;
            if (gameModeController != null) {
                level = gameModeController.getCurrentLevel();
            }
            statsPanel.updateLevel(level);
            statsPanel.updateHighScore(state.getHighScore());
        }

        if (statsPanelRight != null) {
            if (gameModeController != null) {
                int targetLines = gameModeController.getTargetLines();
                int currentLines = gameModeController.getLinesCleared();
                statsPanelRight.updateTarget(currentLines, targetLines);
            } else {
                statsPanelRight.updateTarget(0, 0);
            }
        }
    }

    public void startTimer(GameState state) {
        if (state != null) {
            state.setGameStartTimeNow();
            state.resetElapsedTime();
        }
    }

    public void updateTime(GameState state, StatsPanel statsPanel) {
        if (statsPanel == null || state == null) return;

        long totalSeconds = state.getElapsedSeconds();
        long minutes = totalSeconds / 60;
        long seconds = totalSeconds % 60;

        String timeString = String.format("%02d:%02d", minutes, seconds);
        statsPanel.updateTime(timeString);
    }
}