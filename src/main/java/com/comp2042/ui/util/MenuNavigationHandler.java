package com.comp2042.ui.util;

import com.comp2042.config.KeyBindingsConfig;
import javafx.scene.control.Button;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;

/**
 * Utility class for handling menu keyboard navigation.
 * Provides shared functionality to eliminate duplicate code across menu classes.
 */
public final class MenuNavigationHandler {

    private MenuNavigationHandler() {}

    /**
     * Parses a KeyEvent and returns a NavigationInput object containing boolean flags for navigation actions.
     * 
     * @param event The key event to parse
     * @return NavigationInput with parsed action flags
     */
    public static NavigationInput parseKeyPress(KeyEvent event) {
        KeyCode code = event.getCode();
        KeyBindingsConfig config = KeyBindingsConfig.getInstance();
        KeyBindingsConfig.Action action = config.getAction(code);

        boolean isLeft = (code == KeyCode.LEFT) || (action == KeyBindingsConfig.Action.MOVE_LEFT);
        boolean isRight = (code == KeyCode.RIGHT) || (action == KeyBindingsConfig.Action.MOVE_RIGHT);
        boolean isUp = (code == KeyCode.UP) || (action == KeyBindingsConfig.Action.ROTATE);
        boolean isDown = (code == KeyCode.DOWN) || (action == KeyBindingsConfig.Action.SOFT_DROP);
        boolean isSelect = (code == KeyCode.ENTER || code == KeyCode.SPACE) || (action == KeyBindingsConfig.Action.HARD_DROP);
        boolean isBack = (code == KeyCode.ESCAPE) || (action == KeyBindingsConfig.Action.PAUSE);

        return new NavigationInput(isUp, isDown, isLeft, isRight, isSelect, isBack, event);
    }

    /**
     * Updates button styles based on selected index.
     * Adds "selected" style class to the selected button and removes it from others.
     * 
     * @param buttons Array of buttons to update
     * @param selectedIndex Index of the currently selected button
     */
    public static void updateButtonStyles(Button[] buttons, int selectedIndex) {
        if (buttons == null) return;
        for (int i = 0; i < buttons.length; i++) {
            Button btn = buttons[i];
            if (btn == null) continue;
            if (i == selectedIndex) {
                if (!btn.getStyleClass().contains("selected")) {
                    btn.getStyleClass().add("selected");
                }
            } else {
                btn.getStyleClass().remove("selected");
            }
        }
    }

    /**
     * Handles simple vertical navigation for menus with a button list.
     * Wraps around at the top and bottom.
     * 
     * @param input Parsed navigation input
     * @param buttons Array of buttons to navigate
     * @param selectedIndex Current selected index (will be modified)
     * @return New selected index, or -1 if no navigation occurred
     */
    public static int handleVerticalNavigation(NavigationInput input, Button[] buttons, int selectedIndex) {
        if (buttons == null || buttons.length == 0) return -1;

        if (input.isUp()) {
            int newIndex = (selectedIndex - 1 + buttons.length) % buttons.length;
            updateButtonStyles(buttons, newIndex);
            input.getEvent().consume();
            return newIndex;
        } else if (input.isDown()) {
            int newIndex = (selectedIndex + 1) % buttons.length;
            updateButtonStyles(buttons, newIndex);
            input.getEvent().consume();
            return newIndex;
        } else if (input.isSelect()) {
            if (selectedIndex >= 0 && selectedIndex < buttons.length && buttons[selectedIndex] != null) {
                buttons[selectedIndex].fire();
                input.getEvent().consume();
                return selectedIndex; // Return same index after firing
            }
        }

        return -1; 
    }

    /**
     * Handles simple vertical navigation with back button support.
     * 
     * @param input Parsed navigation input
     * @param buttons Array of buttons to navigate
     * @param selectedIndex Current selected index (will be modified)
     * @param onBack Callback to execute when back is pressed
     * @return New selected index, or -1 if no navigation occurred
     */
    public static int handleVerticalNavigationWithBack(NavigationInput input, Button[] buttons, 
                                                       int selectedIndex, Runnable onBack) {
        if (input.isBack() && onBack != null) {
            input.getEvent().consume();
            onBack.run();
            return selectedIndex; 
        }

        return handleVerticalNavigation(input, buttons, selectedIndex);
    }
}

