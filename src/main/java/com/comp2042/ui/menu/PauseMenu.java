package com.comp2042.ui.menu;

import com.comp2042.ui.util.MenuNavigationHandler;
import com.comp2042.ui.util.NavigationInput;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.VBox;

/**
 * Pause menu UI component with resume, restart, and main menu options.
 */
public class PauseMenu extends VBox {

    private Button resumeButton;
    private Button restartButton;
    private Button mainMenuButton;
    private int selectedIndex = 0;
    private Button[] buttons;

    public PauseMenu() {
        getStylesheets().add(getClass().getResource("/menu_style.css").toExternalForm());
        getStyleClass().add("menu-container");
        setAlignment(Pos.CENTER);
        setSpacing(15);
        setPadding(new Insets(30, 50, 30, 50));

        Label titleLabel = new Label("PAUSED");
        titleLabel.getStyleClass().add("menu-title-red");

        // Resume button
        resumeButton = createButton("Resume");
        resumeButton.setOnAction(e -> {
            if (onResume != null) {
                onResume.run();
            }
        });
        resumeButton.setOnMouseEntered(e -> setSelectedIndex(0));

        // Restart button
        restartButton = createButton("Restart");
        restartButton.setOnAction(e -> {
            if (onRestart != null) {
                onRestart.run();
            }
        });
        restartButton.setOnMouseEntered(e -> setSelectedIndex(1));

        // Main Menu button
        mainMenuButton = createButton("Main Menu");
        mainMenuButton.setOnAction(e -> {
            if (onMainMenu != null) {
                onMainMenu.run();
            }
        });
        mainMenuButton.setOnMouseEntered(e -> setSelectedIndex(2));

        buttons = new Button[]{resumeButton, restartButton, mainMenuButton};
        updateButtonStyles();

        getChildren().addAll(titleLabel, resumeButton, restartButton, mainMenuButton);

        // Handle keyboard navigation
        setOnKeyPressed(this::handleKeyPress);
        setFocusTraversable(true);
        setPrefWidth(350);
        setPrefHeight(350);
    }

    private Button createButton(String text) {
        Button button = new Button(text);
        button.getStyleClass().add("menu-button");
        return button;
    }

    private void handleKeyPress(KeyEvent event) {
        NavigationInput input = MenuNavigationHandler.parseKeyPress(event);
        int newIndex = MenuNavigationHandler.handleVerticalNavigationWithBack(input, buttons, selectedIndex, 
                input.isBack() ? onResume : null);
        if (newIndex >= 0) {
            selectedIndex = newIndex;
        }
    }

    private void updateButtonStyles() {
        MenuNavigationHandler.updateButtonStyles(buttons, selectedIndex);
    }

    public void setSelectedIndex(int index) {
        if (index >= 0 && index < buttons.length) {
            selectedIndex = index;
            updateButtonStyles();
        }
    }

    public int getSelectedIndex() {
        return selectedIndex;
    }

    // Callbacks
    private Runnable onResume;
    private Runnable onRestart;
    private Runnable onMainMenu;

    public void setOnResume(Runnable onResume) {
        this.onResume = onResume;
    }

    public void setOnRestart(Runnable onRestart) {
        this.onRestart = onRestart;
    }

    public void setOnMainMenu(Runnable onMainMenu) {
        this.onMainMenu = onMainMenu;
    }

    public void requestFocusForNavigation() {
        requestFocus();
        setSelectedIndex(0);
    }
}