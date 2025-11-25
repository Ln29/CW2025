package com.comp2042.ui.menu;

import com.comp2042.ui.util.MenuNavigationHandler;
import com.comp2042.ui.util.NavigationInput;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.image.Image;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.*;
import javafx.scene.paint.Color;
import javafx.scene.text.Font;
import javafx.scene.text.FontWeight;

public class MainMenu extends VBox {

    private Button startButton;
    private Button settingsButton;
    private Button exitButton;
    private int selectedIndex = 0;
    private Button[] buttons;

    public MainMenu() {
        getStylesheets().add(getClass().getResource("/menu_style.css").toExternalForm());

        // Set background image
        try {
            Image bgImage = new Image(getClass().getClassLoader().getResource("assets/images/main_menu.png").toExternalForm());
            BackgroundImage backgroundImage = new BackgroundImage(
                    bgImage,
                    BackgroundRepeat.NO_REPEAT,
                    BackgroundRepeat.NO_REPEAT,
                    BackgroundPosition.CENTER,
                    new BackgroundSize(BackgroundSize.AUTO, BackgroundSize.AUTO, false, false, true, true)
            );
            setBackground(new Background(backgroundImage));
        } catch (Exception e) {
            getStyleClass().add("menu-container");
        }

        setAlignment(Pos.CENTER);
        setSpacing(15);
        setPadding(new Insets(30, 50, 30, 50));
        setPrefWidth(900);
        setPrefHeight(700);

        setViewOrder(-1);

        // Load custom font for title
        Font titleFont;
        try {
            titleFont = Font.loadFont(getClass().getResourceAsStream("/assets/fonts/PressStart2P-Regular.ttf"), 55);
            if (titleFont == null) {
                throw new Exception("Font file not found");
            }
        } catch (Exception e) {
            System.err.println("Could not load PressStart2P font: " + e.getMessage());
            titleFont = Font.font("Arial", FontWeight.BOLD, 48);
        }

        Label titleLabel = new Label("TETRIS 29");
        titleLabel.setTextFill(Color.FIREBRICK);
        titleLabel.setFont(titleFont);
        titleLabel.setPadding(new Insets(0, 0, 40, 0));

        // Start button
        startButton = createButton("Start");
        startButton.setOnAction(e -> {
            if (onStart != null) {
                onStart.run();
            }
        });
        startButton.setOnMouseEntered(e -> setSelectedIndex(0));

        // Settings button
        settingsButton = createButton("Settings");
        settingsButton.setOnAction(e -> {
            if (onSettings != null) {
                onSettings.run();
            }
        });
        settingsButton.setOnMouseEntered(e -> setSelectedIndex(1));

        // Exit button
        exitButton = createButton("Exit");
        exitButton.setOnAction(e -> {
            if (onExit != null) {
                onExit.run();
            }
        });
        exitButton.setOnMouseEntered(e -> setSelectedIndex(2));

        buttons = new Button[]{startButton, settingsButton, exitButton};
        updateButtonStyles();

        getChildren().addAll(titleLabel, startButton, settingsButton, exitButton);

        setOnKeyPressed(this::handleKeyPress);
        setFocusTraversable(true);
    }

    private Button createButton(String text) {
        Button button = new Button(text);
        button.getStyleClass().add("menu-button");
        return button;
    }

    private void handleKeyPress(KeyEvent event) {
        NavigationInput input = MenuNavigationHandler.parseKeyPress(event);
        int newIndex = MenuNavigationHandler.handleVerticalNavigation(input, buttons, selectedIndex);
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

    private Runnable onStart;
    private Runnable onSettings;
    private Runnable onExit;

    public void setOnStart(Runnable onStart) {
        this.onStart = onStart;
    }

    public void setOnSettings(Runnable onSettings) {
        this.onSettings = onSettings;
    }

    public void setOnExit(Runnable onExit) {
        this.onExit = onExit;
    }

    public void requestFocusForNavigation() {
        requestFocus();
        setSelectedIndex(0);
    }
}