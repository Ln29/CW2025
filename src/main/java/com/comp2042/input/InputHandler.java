package com.comp2042.input;

import com.comp2042.config.KeyBindingsConfig;
import com.comp2042.controller.GameLifecycle;
import com.comp2042.ui.menu.MenuController;
import javafx.beans.property.BooleanProperty;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.BorderPane;

/**
 * Handles all keyboard input: action map initialization, key event routing,
 * pause detection/toggling, overlay routing, and key binding lookups.
 */
public class InputHandler {

    /**
     * Interface defining game action callbacks.
     */
    public interface InputActions {
        void moveLeft();
        void moveRight();
        void rotate();
        void softDrop();
        void hardDrop();
        void hold();
    }

    private final GridPane gamePanel;
    private final BorderPane gameBoardRoot;
    private final KeyBindingsConfig keyBindingsConfig;
    private final MenuController menuController;
    private final GameLifecycle gameLifecycle;
    private final BooleanProperty isPause;
    private final BooleanProperty isGameOver;

    private final java.util.Map<KeyBindingsConfig.Action, Runnable> actionHandlers;

    /**
     * Creates an input handler with specified components.
     * 
     * @param gamePanel game panel to attach key listeners
     * @param gameBoardRoot root game board container
     * @param keyBindingsConfig key bindings configuration
     * @param menuController menu controller for overlay routing
     * @param gameLifecycle game lifecycle for pause/resume
     * @param isPause pause state property
     * @param isGameOver game over state property
     * @param actions game action callbacks
     */
    public InputHandler(
            GridPane gamePanel,
            BorderPane gameBoardRoot,
            KeyBindingsConfig keyBindingsConfig,
            MenuController menuController,
            GameLifecycle gameLifecycle,
            BooleanProperty isPause,
            BooleanProperty isGameOver,
            InputActions actions
    ) {
        this.gamePanel = gamePanel;
        this.gameBoardRoot = gameBoardRoot;
        this.keyBindingsConfig = keyBindingsConfig;
        this.menuController = menuController;
        this.gameLifecycle = gameLifecycle;
        this.isPause = isPause;
        this.isGameOver = isGameOver;
        this.actionHandlers = buildActionHandlers(actions);
    }

    private java.util.Map<KeyBindingsConfig.Action, Runnable> buildActionHandlers(InputActions actions) {
        java.util.Map<KeyBindingsConfig.Action, Runnable> handlers =
                new java.util.EnumMap<>(KeyBindingsConfig.Action.class);
        handlers.put(KeyBindingsConfig.Action.MOVE_LEFT, actions::moveLeft);
        handlers.put(KeyBindingsConfig.Action.MOVE_RIGHT, actions::moveRight);
        handlers.put(KeyBindingsConfig.Action.ROTATE, actions::rotate);
        handlers.put(KeyBindingsConfig.Action.SOFT_DROP, actions::softDrop);
        handlers.put(KeyBindingsConfig.Action.HARD_DROP, actions::hardDrop);
        handlers.put(KeyBindingsConfig.Action.HOLD, actions::hold);
        return handlers;
    }

    /**
     * Attaches key event handler to the game panel.
     */
    public void attach() {
        gamePanel.setOnKeyPressed(this::handleKeyPressed);
    }

    private void handleKeyPressed(KeyEvent keyEvent) {
        KeyCode code = keyEvent.getCode();

        // Pause key detection (via InputRouter)
        boolean kbVisible = menuController != null && menuController.isKeyBindingsMenuVisible();
        if (InputRouter.shouldTogglePause(keyEvent, keyBindingsConfig,
                Boolean.TRUE.equals(isGameOver.getValue()), kbVisible)) {
            // Only allow pause/resume toggling when actually in gameplay (no overlay) or in PAUSE menu.
            if (menuController != null) {
                InputRouter.Overlay overlay = menuController.getActiveOverlay();
                boolean inGameplayContext = (overlay == InputRouter.Overlay.NONE || overlay == InputRouter.Overlay.PAUSE);
                if (!inGameplayContext) {
                    // Ignore pause/resume toggles when overlays like MAIN_MENU/SETTINGS/THEME/KEY_BINDINGS are active
                    return;
                }
            }
            if (gameLifecycle == null || !gameLifecycle.hasTimers()) {
                return;
            }
            if (Boolean.TRUE.equals(isPause.getValue())) {
                if (gameLifecycle != null) {
                    gameLifecycle.resumeTimers();
                    isPause.setValue(Boolean.FALSE);
                }
                if (menuController != null) {
                    menuController.hidePauseMenu();
                }
            } else {
                if (gameLifecycle != null) {
                    gameLifecycle.pauseTimers();
                    isPause.setValue(Boolean.TRUE);
                }
                if (menuController != null) {
                    menuController.showPauseMenu(gameBoardRoot);
                }
            }
            return;
        }

        // Route events to active overlay
        if (menuController != null && menuController.routeKey(keyEvent)) {
            return;
        }

        // Handle game controls via action map
        if (Boolean.FALSE.equals(isPause.getValue()) && Boolean.FALSE.equals(isGameOver.getValue())) {
            KeyBindingsConfig.Action action = keyBindingsConfig.getAction(code);
            java.util.Optional.ofNullable(actionHandlers.get(action)).ifPresent(r -> {
                r.run();
                keyEvent.consume();
            });
        }
    }
}

