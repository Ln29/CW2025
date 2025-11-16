package com.comp2042.config;

public final class GameConstants {
    
    private GameConstants() {}

    // Board/grid
    public static final int BRICK_SIZE = 30;
    public static final int HIDDEN_ROW_COUNT = 2; // Top hidden rows
    public static final int BOARD_COLS = 10;
    public static final int BOARD_VISIBLE_ROWS = 20;

    // Border padding around board (12px each side)
    public static final int BOARD_BORDER_TOTAL_PX = 24;

    // Timings (ms)
    public static final int GAME_TICK_MS = 400;
    public static final int LOCK_CHECK_MS = 50;

    // UI styling
    public static final int RECT_ARC_RADIUS = 9;
    public static final int GRID_STROKE_WIDTH = 1;
    public static final int GHOST_STROKE_WIDTH = 2;

    // Notifications
    public static final int NOTIFICATION_MAX = 5;

    // Sound effects file names
    public static final String SFX_CLICK = "click.wav";
    public static final String SFX_CLEAR_LINE = "clearline.wav";
    public static final String SFX_BLOCK_FALL = "blockfall.wav";
    public static final String SFX_LOSE = "lose.wav";
} 


