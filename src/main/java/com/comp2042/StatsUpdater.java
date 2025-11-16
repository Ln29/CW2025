package com.comp2042;

public class StatsUpdater {

    public void updateAllStats(GameState state, Board board, StatsPanel statsPanel, StatsPanelRight statsPanelRight) {
        if (state == null) return;

        // Update high score from current score if higher
        if (board != null && board.getScore() != null && board.getScore().scoreProperty() != null) {
            int currentScore = board.getScore().scoreProperty().getValue();
            state.setHighScore(currentScore);
        }

        if (statsPanel != null) {
            statsPanel.updateLines(state.getTotalLinesCleared());
            // Level not tracked; keep 1 for now
            statsPanel.updateLevel(1);
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