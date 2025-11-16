package com.comp2042;

public class MenuFactory {
    private final AudioManager audioManager;
    private final MenuCallbacks callbacks;

    private MainMenu mainMenu;
    private SettingsMenu settingsMenu;
    private KeyBindingsMenu keyBindingsMenu;
    private ThemeMenu themeMenu;
    private PauseMenu pauseMenu;
    private GameOverMenu gameOverMenu;

    public MenuFactory(AudioManager audioManager, MenuCallbacks callbacks) {
        this.audioManager = audioManager;
        this.callbacks = callbacks;
    }

    public MainMenu ensureMainMenu() {
        if (mainMenu != null) return mainMenu;
        mainMenu = new MainMenu();
        mainMenu.setVisible(false);
        mainMenu.setOnStart(() -> {
            click();
            callbacks.onStartGame();
        });
        mainMenu.setOnSettings(() -> {
            click();
            callbacks.onOpenSettings();
        });
        mainMenu.setOnExit(() -> {
            click();
            callbacks.onExitGame();
        });
        return mainMenu;
    }

    public SettingsMenu ensureSettingsMenu() {
        if (settingsMenu != null) return settingsMenu;
        settingsMenu = new SettingsMenu();
        settingsMenu.setVisible(false);
        settingsMenu.setOnKeyBindings(() -> {
            click();
            callbacks.onOpenKeyBindings();
        });
        settingsMenu.setOnThemeSelection(() -> {
            click();
            callbacks.onOpenThemes();
        });
        settingsMenu.setOnBack(() -> {
            click();
            callbacks.onBackFromSettings();
        });
        return settingsMenu;
    }

    public KeyBindingsMenu ensureKeyBindingsMenu() {
        if (keyBindingsMenu != null) return keyBindingsMenu;
        keyBindingsMenu = new KeyBindingsMenu();
        keyBindingsMenu.setVisible(false);
        keyBindingsMenu.setOnBack(() -> {
            click();
            callbacks.onBackFromKeyBindings();
        });
        keyBindingsMenu.setOnBindingsChanged(callbacks::onBindingsChanged);
        return keyBindingsMenu;
    }

    public ThemeMenu ensureThemeMenu() {
        if (themeMenu != null) return themeMenu;
        themeMenu = new ThemeMenu();
        themeMenu.setVisible(false);
        themeMenu.setOnBack(() -> {
            click();
            callbacks.onBackFromTheme();
        });
        themeMenu.setOnThemeSelected(theme -> {
            click();
            callbacks.onThemeSelected(theme);
        });
        return themeMenu;
    }

    public PauseMenu ensurePauseMenu() {
        if (pauseMenu != null) return pauseMenu;
        pauseMenu = new PauseMenu();
        pauseMenu.setVisible(false);
        pauseMenu.setOnResume(() -> {
            click();
            callbacks.onResumeGame();
        });
        pauseMenu.setOnRestart(() -> {
            click();
            callbacks.onRestartGame();
        });
        pauseMenu.setOnMainMenu(() -> {
            click();
            callbacks.onOpenMainMenuFromPause();
        });
        return pauseMenu;
    }

    public GameOverMenu ensureGameOverMenu() {
        if (gameOverMenu != null) return gameOverMenu;
        gameOverMenu = new GameOverMenu();
        gameOverMenu.setVisible(false);
        gameOverMenu.setOnRestart(() -> {
            click();
            callbacks.onRestartFromGameOver();
        });
        gameOverMenu.setOnMainMenu(() -> {
            click();
            callbacks.onOpenMainMenuFromGameOver();
        });
        return gameOverMenu;
    }

    private void click() {
        if (audioManager != null) {
            audioManager.playSoundEffect(GameConstants.SFX_CLICK);
        }
    }
}