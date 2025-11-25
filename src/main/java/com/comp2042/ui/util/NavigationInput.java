package com.comp2042.ui.util;

import javafx.scene.input.KeyEvent;

/**
 * Represents parsed keyboard input for menu navigation.
 * Extracted from menu classes to eliminate duplicate key mapping code.
 */
public final class NavigationInput {
    private final boolean isUp;
    private final boolean isDown;
    private final boolean isLeft;
    private final boolean isRight;
    private final boolean isSelect;
    private final boolean isBack;
    private final KeyEvent event;

    NavigationInput(boolean isUp, boolean isDown, boolean isLeft, boolean isRight, 
                   boolean isSelect, boolean isBack, KeyEvent event) {
        this.isUp = isUp;
        this.isDown = isDown;
        this.isLeft = isLeft;
        this.isRight = isRight;
        this.isSelect = isSelect;
        this.isBack = isBack;
        this.event = event;
    }

    public boolean isUp() {
        return isUp;
    }

    public boolean isDown() {
        return isDown;
    }

    public boolean isLeft() {
        return isLeft;
    }

    public boolean isRight() {
        return isRight;
    }

    public boolean isSelect() {
        return isSelect;
    }

    public boolean isBack() {
        return isBack;
    }

    public KeyEvent getEvent() {
        return event;
    }
}

