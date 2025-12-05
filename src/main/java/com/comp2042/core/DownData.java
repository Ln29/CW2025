package com.comp2042.core;

/**
 * Data container returned from downward movement events.
 * Contains row clearing information and updated view data.
 */
public final class DownData {
    private final ClearRow clearRow;
    private final ViewData viewData;

    /**
     * Creates down data with row clearing and view information.
     * 
     * @param clearRow row clearing result, or null if no rows cleared
     * @param viewData updated view data after movement
     */
    public DownData(ClearRow clearRow, ViewData viewData) {
        this.clearRow = clearRow;
        this.viewData = viewData;
    }

    public ClearRow getClearRow() {
        return clearRow;
    }

    public ViewData getViewData() {
        return viewData;
    }
}
