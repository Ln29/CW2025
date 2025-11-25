package com.comp2042.ui;

import com.comp2042.controller.GameModeController;
import com.comp2042.controller.GameState;
import com.comp2042.core.Board;
import com.comp2042.core.DownData;
import com.comp2042.ui.panels.PanelManager;

/**
 * Handles line clear logic including score updates, notifications, and stats updates.
 */
public class LineClearHandler {
    
    private final GameState gameState;
    private final GameModeController gameModeController;
    private final NotificationService notificationService;
    private final Board board;
    private final PanelManager panelManager;
    
    private static final int COMBO_REWARD_BASE = 50;
    
    public LineClearHandler(
            GameState gameState,
            GameModeController gameModeController,
            NotificationService notificationService,
            Board board,
            PanelManager panelManager
    ) {
        this.gameState = gameState;
        this.gameModeController = gameModeController;
        this.notificationService = notificationService;
        this.board = board;
        this.panelManager = panelManager;
    }
    
    /**
     * Handles line clear logic after a move that may have cleared lines.
     * 
     * @param downData The data from the move operation
     * @param isHardDrop Whether this was a hard drop
     */
    public void handleLineClear(DownData downData, boolean isHardDrop) {
        if (downData == null || downData.getClearRow() == null) {
            return;
        }
        
        int removed = downData.getClearRow().getLinesRemoved();
        int bonus = downData.getClearRow().getScoreBonus();
        
        // Update game state and mode controller if lines were cleared
        if (removed > 0) {
            if (gameState != null) {
                gameState.addClearedLines(removed);
            }
            
            if (gameModeController != null) {
                gameModeController.onLinesCleared(removed);

                if (notificationService != null) {
                    notificationService.checkLevelUp(gameModeController.getCurrentLevel());
                }
            }
        }
        
        // notifications
        if (notificationService != null) {
            notificationService.onLinesCleared(removed, bonus);
            
            // combo rewards
            if (board != null && board.getScore() != null && removed > 0) {
                int comboCount = notificationService.getComboCount();
                int comboReward = calculateComboReward(comboCount, bonus, isHardDrop);
                
                if (comboReward > 0) {
                    board.getScore().add(comboReward);
                }
            }
        }
        
        // stats panels
        if (panelManager != null) {
            panelManager.updateStatsPanels();
        }
    }
    
    /**
     * Calculates combo reward based on combo count and whether it was a hard drop.
     * 
     * @param comboCount Current combo count
     * @param bonus Base score bonus from line clear
     * @param isHardDrop Whether this was a hard drop
     * @return Combo reward to add to score
     */
    private int calculateComboReward(int comboCount, int bonus, boolean isHardDrop) {
        if (isHardDrop) {
            return comboCount > 1 ? bonus * (comboCount - 1) : 0;
        } else {
            return comboCount > 0 ? comboCount * COMBO_REWARD_BASE : 0;
        }
    }
}

