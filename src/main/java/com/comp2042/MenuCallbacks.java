package com.comp2042;

public interface MenuCallbacks {
    // Main menu
    void onStartGame();
    void onOpenSettings();
    void onExitGame();

    // Settings menu
    void onOpenKeyBindings();
    void onOpenThemes();
    void onBackFromSettings();

    // Key bindings menu
    void onBackFromKeyBindings();
    void onBindingsChanged();

    // Theme menu
    void onBackFromTheme();
    void onThemeSelected(ThemeMenu.Theme theme);

    // Pause menu
    void onResumeGame();
    void onRestartGame();
    void onOpenMainMenuFromPause();

    // Game over menu
    void onRestartFromGameOver();
    void onOpenMainMenuFromGameOver();
}