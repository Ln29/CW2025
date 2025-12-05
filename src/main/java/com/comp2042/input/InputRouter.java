package com.comp2042.input;

import com.comp2042.config.KeyBindingsConfig;
import com.comp2042.ui.menu.GameOverMenu;
import com.comp2042.ui.menu.KeyBindingsMenu;
import com.comp2042.ui.menu.MainMenu;
import com.comp2042.ui.menu.PauseMenu;
import com.comp2042.ui.menu.SettingsMenu;
import com.comp2042.ui.menu.ThemeMenu;
import javafx.scene.input.KeyEvent;

/**
 * Utility class for routing keyboard input to appropriate menus or game controls.
 * Handles pause detection and overlay-based input routing.
 */
public final class InputRouter {

    private InputRouter() {}

    /**
     * Enumeration of active UI overlays that can receive input.
     */
    public enum Overlay {
        NONE,
        MAIN_MENU,
        SETTINGS,
        KEY_BINDINGS,
        THEME,
        PAUSE,
        GAME_OVER
    }

    /**
     * Checks if the key event should toggle pause state.
     * 
     * @param keyEvent the key event
     * @param bindings key bindings configuration
     * @param isGameOver whether game is over
     * @param keyBindingsVisible whether key bindings menu is visible
     * @return true if pause should be toggled, false otherwise
     */
    public static boolean shouldTogglePause(KeyEvent keyEvent,
                                            KeyBindingsConfig bindings,
                                            boolean isGameOver,
                                            boolean keyBindingsVisible) {
        if (isGameOver) return false;
        final var code = keyEvent.getCode();
        KeyBindingsConfig.Action mapped = bindings != null ? bindings.getAction(code) : null;
        boolean isPauseKey = (code == javafx.scene.input.KeyCode.ESCAPE) || (mapped == KeyBindingsConfig.Action.PAUSE);
        if (isPauseKey && !keyBindingsVisible) {
            keyEvent.consume();
            return true;
        }
        return false;
    }

    /**
     * Routes key event to the appropriate menu based on active overlay.
     * 
     * @param overlay active overlay type
     * @param keyEvent the key event to route
     * @param mainMenu main menu instance
     * @param settingsMenu settings menu instance
     * @param keyBindingsMenu key bindings menu instance
     * @param themeMenu theme menu instance
     * @param pauseMenu pause menu instance
     * @param gameOverMenu game over menu instance
     * @return true if event was routed, false otherwise
     */
    public static boolean route(Overlay overlay,
                                KeyEvent keyEvent,
                                MainMenu mainMenu,
                                SettingsMenu settingsMenu,
                                KeyBindingsMenu keyBindingsMenu,
                                ThemeMenu themeMenu,
                                PauseMenu pauseMenu,
                                GameOverMenu gameOverMenu) {
        switch (overlay) {
            case MAIN_MENU:
                if (mainMenu != null) {
                    mainMenu.fireEvent(keyEvent);
                    keyEvent.consume();
                    return true;
                }
                break;
            case SETTINGS:
                if (settingsMenu != null) {
                    settingsMenu.fireEvent(keyEvent);
                    keyEvent.consume();
                    return true;
                }
                break;
            case KEY_BINDINGS:
                if (keyBindingsMenu != null) {
                    keyBindingsMenu.fireEvent(keyEvent);
                    keyEvent.consume();
                    return true;
                }
                break;
            case THEME:
                if (themeMenu != null) {
                    themeMenu.fireEvent(keyEvent);
                    keyEvent.consume();
                    return true;
                }
                break;
            case PAUSE:
                if (pauseMenu != null) {
                    pauseMenu.fireEvent(keyEvent);
                    keyEvent.consume();
                    return true;
                }
                break;
            case GAME_OVER:
                if (gameOverMenu != null) {
                    gameOverMenu.fireEvent(keyEvent);
                    keyEvent.consume();
                    return true;
                }
                break;
            case NONE:
            default:
                break;
        }
        return false;
    }
}


