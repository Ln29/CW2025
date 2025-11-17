package com.comp2042.ui.menu;

import com.comp2042.config.KeyBindingsConfig;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.effect.DropShadow;
import javafx.scene.image.Image;
import javafx.scene.input.KeyCode;
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
            // If image not found, use solid background
            setStyle("-fx-background-color: rgba(0, 0, 0, 0.8);");
        }
        
        setAlignment(Pos.CENTER);
        setSpacing(15);
        setPadding(new Insets(30, 50, 30, 50));
        setPrefWidth(900);
        setPrefHeight(700);
        
        // Ensure the main menu is always on top
        setViewOrder(-1);

        Font titleFont;
        try {
            titleFont = Font.loadFont(getClass().getResourceAsStream("/assets/fonts/PressStart2P-Regular.ttf"), 55);
            if (titleFont == null) {
                throw new Exception("Font file not found");
            }
        } catch (Exception e) {
            System.err.println("Could not load PressStart2P font: " + e.getMessage());
            titleFont = Font.font("Arial", FontWeight.BOLD, 55);
        }

        // Title
        Label titleLabel = new Label("TETRIS 29");
        titleLabel.setTextFill(Color.BLACK);
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
        
        // Handle keyboard navigation
        setOnKeyPressed(this::handleKeyPress);
        setFocusTraversable(true);
    }
    
    private Button createButton(String text) {
        Button button = new Button(text);
        button.setPrefWidth(200);
        button.setPrefHeight(50);
        button.setFont(Font.font("Arial", FontWeight.BOLD, 18));
        button.setStyle("-fx-background-color: rgba(100, 100, 100, 0.7); " +
                       "-fx-text-fill: white; " +
                       "-fx-background-radius: 5; " +
                       "-fx-border-color: rgba(200, 200, 200, 0.5); " +
                       "-fx-border-width: 2; " +
                       "-fx-border-radius: 5;");
        
        // Hover effects
        button.setOnMouseEntered(e -> {
            if (button != buttons[selectedIndex]) {
                button.setStyle("-fx-background-color: rgba(120, 120, 120, 0.8); " +
                               "-fx-text-fill: white; " +
                               "-fx-background-radius: 5; " +
                               "-fx-border-color: rgba(200, 200, 200, 0.7); " +
                               "-fx-border-width: 2; " +
                               "-fx-border-radius: 5;");
            }
        });
        
        button.setOnMouseExited(e -> {
            if (button != buttons[selectedIndex]) {
                button.setStyle("-fx-background-color: rgba(100, 100, 100, 0.7); " +
                               "-fx-text-fill: white; " +
                               "-fx-background-radius: 5; " +
                               "-fx-border-color: rgba(200, 200, 200, 0.5); " +
                               "-fx-border-width: 2; " +
                               "-fx-border-radius: 5;");
            }
        });
        
        return button;
    }
    
    private void handleKeyPress(KeyEvent event) {
        KeyCode code = event.getCode();
        KeyBindingsConfig config = KeyBindingsConfig.getInstance();
        
        // Check if the pressed key matches any bound action
        KeyBindingsConfig.Action action = config.getAction(code);
        boolean isUp = (code == KeyCode.UP) || (action == KeyBindingsConfig.Action.ROTATE);
        boolean isDown = (code == KeyCode.DOWN) || (action == KeyBindingsConfig.Action.SOFT_DROP);
        boolean isSelect = (code == KeyCode.ENTER || code == KeyCode.SPACE) || (action == KeyBindingsConfig.Action.HARD_DROP);
        
        if (isUp) {
            selectedIndex = (selectedIndex - 1 + buttons.length) % buttons.length;
            updateButtonStyles();
            event.consume();
        } else if (isDown) {
            selectedIndex = (selectedIndex + 1) % buttons.length;
            updateButtonStyles();
            event.consume();
        } else if (isSelect) {
            buttons[selectedIndex].fire();
            event.consume();
        }
    }
    
    private void updateButtonStyles() {
        for (int i = 0; i < buttons.length; i++) {
            if (i == selectedIndex) {
                // Selected button style
                buttons[i].setStyle("-fx-background-color: rgba(255, 215, 0, 0.9); " +
                                   "-fx-text-fill: black; " +
                                   "-fx-background-radius: 5; " +
                                   "-fx-border-color: rgba(255, 255, 255, 0.9); " +
                                   "-fx-border-width: 3; " +
                                   "-fx-border-radius: 5;");
            } else {
                // Unselected button style
                buttons[i].setStyle("-fx-background-color: rgba(100, 100, 100, 0.7); " +
                                   "-fx-text-fill: white; " +
                                   "-fx-background-radius: 5; " +
                                   "-fx-border-color: rgba(200, 200, 200, 0.5); " +
                                   "-fx-border-width: 2; " +
                                   "-fx-border-radius: 5;");
            }
        }
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

