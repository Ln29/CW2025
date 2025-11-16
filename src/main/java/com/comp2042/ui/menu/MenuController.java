package com.comp2042.ui.menu;

import com.comp2042.audio.AudioManager;
import com.comp2042.input.InputRouter;
import com.comp2042.ui.SceneAccessor;
import javafx.scene.Scene;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.Pane;

public class MenuController {
    private MainMenu mainMenu;
    private PauseMenu pauseMenu;
    private GameOverMenu gameOverMenu;
    private SettingsMenu settingsMenu;
    private KeyBindingsMenu keyBindingsMenu;
    private ThemeMenu themeMenu;
    private ModeSelectionMenu modeSelectionMenu;

    private MenuManager menuManager;
    private final MenuFactory menuFactory;
    private final AudioManager audioManager;
    private InputRouter.Overlay activeOverlay = InputRouter.Overlay.NONE;

    public MenuController(MenuFactory menuFactory, AudioManager audioManager) {
        this.menuFactory = menuFactory;
        this.audioManager = audioManager;
    }

    public void setMenuManager(MenuManager menuManager) {
        this.menuManager = menuManager;
    }

    public InputRouter.Overlay getActiveOverlay() {
        return activeOverlay;
    }

    // Initialization helpers
    public void initializeMainMenu(BorderPane gameBoard) {
        if (mainMenu == null) {
            mainMenu = menuFactory.ensureMainMenu();
            Pane root = SceneAccessor.rootOf(gameBoard);
            if (root != null && !root.getChildren().contains(mainMenu)) {
                root.getChildren().add(mainMenu);
            }
        }
    }

    public void showMainMenu(BorderPane gameBoard) {
        ensureManager(gameBoard);
        initializeMainMenu(gameBoard);
        if (menuManager != null && mainMenu != null) {
            Scene scene = SceneAccessor.sceneOf(gameBoard);
            menuManager.showCenteredOnScene(mainMenu, scene);
            mainMenu.requestFocusForNavigation();
            activeOverlay = InputRouter.Overlay.MAIN_MENU;
            if (audioManager != null) {
                audioManager.playMainMenuMusic();
            }
        }
    }

    public void hideMainMenu() {
        if (menuManager != null && mainMenu != null) {
            menuManager.hideIfVisible(mainMenu);
        }
        if (activeOverlay == InputRouter.Overlay.MAIN_MENU) {
            activeOverlay = InputRouter.Overlay.NONE;
        }
    }

    public void initializePauseMenu(BorderPane gameBoard) {
        if (pauseMenu == null) {
            pauseMenu = menuFactory.ensurePauseMenu();
            Pane root = SceneAccessor.rootOf(gameBoard);
            if (root != null && !root.getChildren().contains(pauseMenu)) {
                root.getChildren().add(pauseMenu);
            }
        }
    }

    public void togglePauseMenu(BorderPane gameBoard) {
        ensureManager(gameBoard);
        initializePauseMenu(gameBoard);
        if (pauseMenu == null || menuManager == null) return;
        if (pauseMenu.isVisible()) {
            menuManager.hideIfVisible(pauseMenu);
            if (activeOverlay == InputRouter.Overlay.PAUSE) {
                activeOverlay = InputRouter.Overlay.NONE;
            }
        } else {
            menuManager.showCenteredOnBoard(pauseMenu);
            pauseMenu.requestFocusForNavigation();
            activeOverlay = InputRouter.Overlay.PAUSE;
        }
    }

    public void showPauseMenu(BorderPane gameBoard) {
        ensureManager(gameBoard);
        initializePauseMenu(gameBoard);
        if (menuManager != null && pauseMenu != null) {
            menuManager.showCenteredOnBoard(pauseMenu);
            pauseMenu.requestFocusForNavigation();
            activeOverlay = InputRouter.Overlay.PAUSE;
        }
    }

    public void hidePauseMenu() {
        if (menuManager != null && pauseMenu != null) {
            menuManager.hideIfVisible(pauseMenu);
        }
        if (activeOverlay == InputRouter.Overlay.PAUSE) {
            activeOverlay = InputRouter.Overlay.NONE;
        }
    }

    public void initializeGameOverMenu(BorderPane gameBoard) {
        if (gameOverMenu == null) {
            gameOverMenu = menuFactory.ensureGameOverMenu();
            Pane root = SceneAccessor.rootOf(gameBoard);
            if (root != null && !root.getChildren().contains(gameOverMenu)) {
                root.getChildren().add(gameOverMenu);
            }
        }
    }

    public void showGameOverMenu(BorderPane gameBoard) {
        ensureManager(gameBoard);
        initializeGameOverMenu(gameBoard);
        if (menuManager != null && gameOverMenu != null) {
            menuManager.showCenteredOnBoard(gameOverMenu);
            gameOverMenu.requestFocusForNavigation();
            activeOverlay = InputRouter.Overlay.GAME_OVER;
        }
    }

    public void initializeSettingsMenu(BorderPane gameBoard) {
        if (settingsMenu == null) {
            settingsMenu = menuFactory.ensureSettingsMenu();
            Pane root = SceneAccessor.rootOf(gameBoard);
            if (root != null && !root.getChildren().contains(settingsMenu)) {
                root.getChildren().add(settingsMenu);
            }
        }
    }

    public void showSettingsMenu(BorderPane gameBoard) {
        ensureManager(gameBoard);
        initializeSettingsMenu(gameBoard);
        if (menuManager != null && settingsMenu != null) {
            Scene scene = SceneAccessor.sceneOf(gameBoard);
            menuManager.showCenteredOnScene(settingsMenu, scene);
            settingsMenu.requestFocusForNavigation();
            activeOverlay = InputRouter.Overlay.SETTINGS;
        }
    }

    public void hideSettingsMenu() {
        if (menuManager != null && settingsMenu != null) {
            menuManager.hideIfVisible(settingsMenu);
        }
        if (activeOverlay == InputRouter.Overlay.SETTINGS) {
            activeOverlay = InputRouter.Overlay.NONE;
        }
    }

    public void initializeKeyBindingsMenu(BorderPane gameBoard) {
        if (keyBindingsMenu == null) {
            keyBindingsMenu = menuFactory.ensureKeyBindingsMenu();
            Pane root = SceneAccessor.rootOf(gameBoard);
            if (root != null && !root.getChildren().contains(keyBindingsMenu)) {
                root.getChildren().add(keyBindingsMenu);
            }
        }
    }

    public void showKeyBindingsMenu(BorderPane gameBoard) {
        ensureManager(gameBoard);
        initializeKeyBindingsMenu(gameBoard);
        if (menuManager != null && keyBindingsMenu != null) {
            Scene scene = SceneAccessor.sceneOf(gameBoard);
            menuManager.showCenteredOnScene(keyBindingsMenu, scene);
            keyBindingsMenu.requestFocusForNavigation();
            activeOverlay = InputRouter.Overlay.KEY_BINDINGS;
        }
    }

    public void hideKeyBindingsMenu() {
        if (menuManager != null && keyBindingsMenu != null) {
            menuManager.hideIfVisible(keyBindingsMenu);
        }
        if (activeOverlay == InputRouter.Overlay.KEY_BINDINGS) {
            activeOverlay = InputRouter.Overlay.NONE;
        }
    }

    public void initializeThemeMenu(BorderPane gameBoard) {
        if (themeMenu == null) {
            themeMenu = menuFactory.ensureThemeMenu();
            Pane root = SceneAccessor.rootOf(gameBoard);
            if (root != null && !root.getChildren().contains(themeMenu)) {
                root.getChildren().add(themeMenu);
            }
        }
    }

    public void showThemeMenu(BorderPane gameBoard) {
        ensureManager(gameBoard);
        initializeThemeMenu(gameBoard);
        if (menuManager != null && themeMenu != null) {
            Scene scene = SceneAccessor.sceneOf(gameBoard);
            menuManager.showCenteredOnScene(themeMenu, scene);
            themeMenu.requestFocusForNavigation();
            activeOverlay = InputRouter.Overlay.THEME;
        }
    }

    public void hideThemeMenu() {
        if (menuManager != null && themeMenu != null) {
            menuManager.hideIfVisible(themeMenu);
        }
        if (activeOverlay == InputRouter.Overlay.THEME) {
            activeOverlay = InputRouter.Overlay.NONE;
        }
    }

    private void ensureManager(BorderPane gameBoard) {
        if (menuManager == null && gameBoard != null) {
            menuManager = MenuManager.ensure(null, gameBoard);
        }
    }

    public boolean isKeyBindingsMenuVisible() {
        return keyBindingsMenu != null && keyBindingsMenu.isVisible();
    }

    public boolean routeKey(javafx.scene.input.KeyEvent event) {
        return InputRouter.route(
                activeOverlay,
                event,
                mainMenu,
                settingsMenu,
                keyBindingsMenu,
                themeMenu,
                pauseMenu,
                gameOverMenu
        );
    }

    public void hideGameOverMenu() {
        if (menuManager != null && gameOverMenu != null) {
            menuManager.hideIfVisible(gameOverMenu);
        }
        if (activeOverlay == InputRouter.Overlay.GAME_OVER) {
            activeOverlay = InputRouter.Overlay.NONE;
        }
    }

    public SettingsMenu getSettingsMenu() {
        return settingsMenu;
    }

    public KeyBindingsMenu getKeyBindingsMenu() {
        return keyBindingsMenu;
    }

    public MainMenu getMainMenu() {
        return mainMenu;
    }

    public PauseMenu getPauseMenu() {
        return pauseMenu;
    }

    public GameOverMenu getGameOverMenu() {
        return gameOverMenu;
    }

    public ThemeMenu getThemeMenu() {
        return themeMenu;
    }

    public void initializeModeSelectionMenu(BorderPane gameBoard) {
        if (modeSelectionMenu == null) {
            modeSelectionMenu = menuFactory.ensureModeSelectionMenu();
            Pane root = SceneAccessor.rootOf(gameBoard);
            if (root != null && !root.getChildren().contains(modeSelectionMenu)) {
                root.getChildren().add(modeSelectionMenu);
            }
        }
    }

    public void showModeSelectionMenu(BorderPane gameBoard) {
        ensureManager(gameBoard);
        initializeModeSelectionMenu(gameBoard);
        if (menuManager != null && modeSelectionMenu != null) {
            Scene scene = SceneAccessor.sceneOf(gameBoard);
            menuManager.showCenteredOnScene(modeSelectionMenu, scene);
            modeSelectionMenu.requestFocus();
            activeOverlay = InputRouter.Overlay.MAIN_MENU; // Use MAIN_MENU overlay for now
        }
    }

    public void hideModeSelectionMenu() {
        if (menuManager != null && modeSelectionMenu != null) {
            menuManager.hideIfVisible(modeSelectionMenu);
        }
        if (activeOverlay == InputRouter.Overlay.MAIN_MENU) {
            activeOverlay = InputRouter.Overlay.NONE;
        }
    }

    public ModeSelectionMenu getModeSelectionMenu() {
        return modeSelectionMenu;
    }
}


