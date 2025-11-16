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
            statsPanel.updateLines(state.getTotalLinesCleared());
            // Get level from game mode controller if available
            int level = 1;
            if (gameModeController != null) {
                level = gameModeController.getCurrentLevel();
            }
            statsPanel.updateLevel(level);
            statsPanel.updateHighScore(state.getHighScore());
        }

        if (statsPanelRight != null && board != null && board.getScore() != null && board.getScore().scoreProperty() != null) {
            statsPanelRight.updateScore(board.getScore().scoreProperty().getValue());
        }
    }

    public void startTimer(GameState state) {
        if (state != null) {
            state.setGameStartTimeNow();
        }
    }

    public void updateTime(GameState state, StatsPanel statsPanel) {
        if (statsPanel == null || state == null || state.getGameStartTime() == 0) return;

        long elapsed = System.currentTimeMillis() - state.getGameStartTime();
        long seconds = elapsed / 1000;
        long minutes = seconds / 60;
        seconds = seconds % 60;

        String timeString = String.format("%02d:%02d", minutes, seconds);
        statsPanel.updateTime(timeString);
    }
}