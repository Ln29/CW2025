package com.comp2042.ui.menu;

public interface MenuCallbacks {
    // Main menu
    void onOpenSettings();
    void onExitGame();

    // Settings menu
    void onOpenKeyBindings();
    void onOpenThemes();
    void onBackFromSettings();

    // Mode selection menu
    void onOpenModeSelection();
    void onModeSelected();
    void onBackFromModeSelection();

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


