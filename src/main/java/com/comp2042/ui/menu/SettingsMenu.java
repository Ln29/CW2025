package com.comp2042.ui.menu;

import com.comp2042.config.KeyBindingsConfig;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.Slider;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;
import javafx.scene.text.Font;
import javafx.scene.text.FontWeight;

public class SettingsMenu extends VBox {
    
    private Button keyBindingsButton;
    private Button themeSelectionButton;
    private Button backButton;
    private int selectedIndex = 0;
    private Button[] buttons;
    
    private Slider masterVolumeSlider;
    private Slider musicVolumeSlider;
    private Slider soundEffectVolumeSlider;
    
    public SettingsMenu() {
        setAlignment(Pos.CENTER);
        setSpacing(20);
        setPadding(new Insets(40, 60, 40, 60));
        setStyle("-fx-background-color: rgba(0, 0, 0, 0.9); -fx-background-radius: 10;");
        
        // Title
        Label titleLabel = new Label("SETTINGS");
        titleLabel.setTextFill(Color.RED);
        titleLabel.setFont(Font.font("Arial", FontWeight.BOLD, 36));
        titleLabel.setPadding(new Insets(0, 0, 20, 0));
        
        // Master Volume
        HBox masterVolumeBox = createVolumeControl("Master Volume", 0, 100, 100);
        masterVolumeSlider = (Slider) masterVolumeBox.getChildren().get(1);
        
        // Music Volume
        HBox musicVolumeBox = createVolumeControl("Music Volume", 0, 100, 100);
        musicVolumeSlider = (Slider) musicVolumeBox.getChildren().get(1);
        
        // Sound Effect Volume
        HBox soundEffectVolumeBox = createVolumeControl("Sound Effect Volume", 0, 100, 100);
        soundEffectVolumeSlider = (Slider) soundEffectVolumeBox.getChildren().get(1);
        
        VBox audioSection = new VBox(15);
        audioSection.setAlignment(Pos.CENTER);
        audioSection.getChildren().addAll(masterVolumeBox, musicVolumeBox, soundEffectVolumeBox);
        
        // === KEY BINDINGS & THEME SELECTION ===
        HBox bottomButtonsBox = new HBox(20);
        bottomButtonsBox.setAlignment(Pos.CENTER);
        bottomButtonsBox.setPadding(new Insets(10, 0, 10, 0));
        
        // Key Bindings button (left)
        keyBindingsButton = createButton("Key Bindings");
        keyBindingsButton.setOnAction(e -> {
            // TODO: Implement key bindings menu
            if (onKeyBindings != null) {
                onKeyBindings.run();
            }
        });
        keyBindingsButton.setOnMouseEntered(e -> setSelectedIndex(0));
        
        // Theme Selection button (right)
        themeSelectionButton = createButton("Themes");
        themeSelectionButton.setOnAction(e -> {
            // TODO: Implement theme selection
            if (onThemeSelection != null) {
                onThemeSelection.run();
            }
        });
        themeSelectionButton.setOnMouseEntered(e -> setSelectedIndex(1));
        
        bottomButtonsBox.getChildren().addAll(keyBindingsButton, themeSelectionButton);
        
        // Back button
        backButton = createButton("Back");
        backButton.setOnAction(e -> {
            if (onBack != null) {
                onBack.run();
            }
        });
        backButton.setOnMouseEntered(e -> setSelectedIndex(2));
        
        buttons = new Button[]{keyBindingsButton, themeSelectionButton, backButton};
        updateButtonStyles();
        
        // Add all content directly to this VBox
        getChildren().addAll(titleLabel, audioSection, bottomButtonsBox, backButton);
        
        // Handle keyboard navigation
        setOnKeyPressed(this::handleKeyPress);
        setFocusTraversable(true);
        setPrefWidth(600);
        setPrefHeight(700);
        setViewOrder(-1);
    }
    
    private HBox createVolumeControl(String labelText, double min, double max, double defaultValue) {
        HBox hbox = new HBox(15);
        hbox.setAlignment(Pos.CENTER_LEFT);
        hbox.setPrefWidth(400);
        
        Label label = new Label(labelText + ":");
        label.setTextFill(Color.WHITE);
        label.setFont(Font.font("Arial", 16));
        label.setPrefWidth(180);
        
        Slider slider = new Slider(min, max, defaultValue);
        slider.setPrefWidth(150);
        slider.setShowTickLabels(false);
        slider.setShowTickMarks(false);
        
        Label valueLabel = new Label(String.valueOf((int) defaultValue));
        valueLabel.setTextFill(Color.WHITE);
        valueLabel.setFont(Font.font("Arial", FontWeight.BOLD, 16));
        valueLabel.setPrefWidth(50);
        valueLabel.setAlignment(Pos.CENTER_RIGHT);
        
        // Update value label when slider changes
        slider.valueProperty().addListener((obs, oldVal, newVal) -> {
            valueLabel.setText(String.valueOf((int) newVal.doubleValue()));
        });
        
        hbox.getChildren().addAll(label, slider, valueLabel);
        return hbox;
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
        boolean isLeft = (code == KeyCode.LEFT) || (action == KeyBindingsConfig.Action.MOVE_LEFT);
        boolean isRight = (code == KeyCode.RIGHT) || (action == KeyBindingsConfig.Action.MOVE_RIGHT);
        boolean isSelect = (code == KeyCode.ENTER || code == KeyCode.SPACE) || (action == KeyBindingsConfig.Action.HARD_DROP);
        boolean isBack = (code == KeyCode.ESCAPE) || (action == KeyBindingsConfig.Action.PAUSE);
        
        if (isUp) {
            selectedIndex = (selectedIndex - 1 + buttons.length) % buttons.length;
            updateButtonStyles();
            event.consume();
        } else if (isDown) {
            selectedIndex = (selectedIndex + 1) % buttons.length;
            updateButtonStyles();
            event.consume();
        } else if (isLeft) {
            // Navigate between Key Bindings and Theme Selection buttons
            if (selectedIndex == 0) {
                selectedIndex = 1;
            } else if (selectedIndex == 1) {
                selectedIndex = 0;
            }
            updateButtonStyles();
            event.consume();
        } else if (isRight) {
            // Navigate between Key Bindings and Theme Selection buttons
            if (selectedIndex == 0) {
                selectedIndex = 1;
            } else if (selectedIndex == 1) {
                selectedIndex = 0;
            }
            updateButtonStyles();
            event.consume();
        } else if (isSelect) {
            buttons[selectedIndex].fire();
            event.consume();
        } else if (isBack) {
            if (onBack != null) {
                onBack.run();
            }
            event.consume();
        }
    }
    
    private void updateButtonStyles() {
        for (int i = 0; i < buttons.length; i++) {
            if (i == selectedIndex) {
                // Selected button style
                buttons[i].setStyle("-fx-background-color: rgba(250, 241, 70, 0.9); " +
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
    
    // Getters for volume sliders
    public Slider getMasterVolumeSlider() {
        return masterVolumeSlider;
    }
    
    public Slider getMusicVolumeSlider() {
        return musicVolumeSlider;
    }
    
    public Slider getSoundEffectVolumeSlider() {
        return soundEffectVolumeSlider;
    }
    
    // Callbacks
    private Runnable onKeyBindings;
    private Runnable onThemeSelection;
    private Runnable onBack;
    
    public void setOnKeyBindings(Runnable onKeyBindings) {
        this.onKeyBindings = onKeyBindings;
    }
    
    public void setOnThemeSelection(Runnable onThemeSelection) {
        this.onThemeSelection = onThemeSelection;
    }
    
    public void setOnBack(Runnable onBack) {
        this.onBack = onBack;
    }
    
    public void requestFocusForNavigation() {
        requestFocus();
        setSelectedIndex(2); // Start with Back button selected
    }
}

