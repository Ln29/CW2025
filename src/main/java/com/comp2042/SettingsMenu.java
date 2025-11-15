package com.comp2042;

import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ScrollPane;
import javafx.scene.control.Slider;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;
import javafx.scene.text.Font;
import javafx.scene.text.FontWeight;

public class SettingsMenu extends VBox {

    private VBox contentBox;

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
        setPadding(new Insets(20));
        setStyle("-fx-background-color: rgba(0, 0, 0, 0.9); -fx-background-radius: 10;");

        //scrollable
        contentBox = new VBox(20);
        contentBox.setAlignment(Pos.TOP_CENTER);
        contentBox.setPadding(new Insets(30, 50, 30, 50));

        Label titleLabel = new Label("SETTINGS");
        titleLabel.setTextFill(Color.RED);
        titleLabel.setFont(Font.font("Arial", FontWeight.BOLD, 36));
        titleLabel.setPadding(new Insets(0, 0, 20, 0));

        HBox masterVolumeBox = createVolumeControl("Master Volume", 0, 100, 100);
        masterVolumeSlider = (Slider) masterVolumeBox.getChildren().get(1);

        HBox musicVolumeBox = createVolumeControl("Music Volume", 0, 100, 100);
        musicVolumeSlider = (Slider) musicVolumeBox.getChildren().get(1);

        HBox soundEffectVolumeBox = createVolumeControl("Sound Effect Volume", 0, 100, 100);
        soundEffectVolumeSlider = (Slider) soundEffectVolumeBox.getChildren().get(1);

        VBox audioSection = new VBox(15);
        audioSection.setAlignment(Pos.CENTER);
        audioSection.getChildren().addAll(masterVolumeBox, musicVolumeBox, soundEffectVolumeBox);

        HBox bottomButtonsBox = new HBox(20);
        bottomButtonsBox.setAlignment(Pos.CENTER);
        bottomButtonsBox.setPadding(new Insets(10, 0, 10, 0));

        // Key Bindings
        keyBindingsButton = createButton("Key Bindings");
        keyBindingsButton.setOnAction(e -> {
            // to do
            if (onKeyBindings != null) {
                onKeyBindings.run();
            }
        });
        keyBindingsButton.setOnMouseEntered(e -> setSelectedIndex(0));

        // Theme Selection
        themeSelectionButton = createButton("Themes");
        themeSelectionButton.setOnAction(e -> {
            // to do
            if (onThemeSelection != null) {
                onThemeSelection.run();
            }
        });
        themeSelectionButton.setOnMouseEntered(e -> setSelectedIndex(1));

        bottomButtonsBox.getChildren().addAll(keyBindingsButton, themeSelectionButton);

        // Back
        backButton = createButton("Back");
        backButton.setOnAction(e -> {
            if (onBack != null) {
                onBack.run();
            }
        });
        backButton.setOnMouseEntered(e -> setSelectedIndex(2));

        buttons = new Button[]{keyBindingsButton, themeSelectionButton, backButton};
        updateButtonStyles();

        // Add all content to contentBox
        contentBox.getChildren().addAll(titleLabel, audioSection, bottomButtonsBox, backButton);

        // Wrap contentBox in ScrollPane for scrollability
        ScrollPane scrollPane = new ScrollPane(contentBox);
        scrollPane.setFitToWidth(true);
        scrollPane.setFitToHeight(false);
        scrollPane.setHbarPolicy(ScrollPane.ScrollBarPolicy.NEVER);
        scrollPane.setVbarPolicy(ScrollPane.ScrollBarPolicy.AS_NEEDED);
        scrollPane.setStyle("-fx-background: transparent; -fx-background-color: transparent;");
        scrollPane.setPadding(new Insets(0));

        getChildren().add(scrollPane);

        //keyboard navigation
        setOnKeyPressed(this::handleKeyPress);
        setFocusTraversable(true);
        setPrefWidth(500);
        setPrefHeight(600);
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

        if (code == KeyCode.UP || code == KeyCode.W) {
            selectedIndex = (selectedIndex - 1 + buttons.length) % buttons.length;
            updateButtonStyles();
            event.consume();
        } else if (code == KeyCode.DOWN || code == KeyCode.S) {
            selectedIndex = (selectedIndex + 1) % buttons.length;
            updateButtonStyles();
            event.consume();
        } else if (code == KeyCode.LEFT || code == KeyCode.A) {
            if (selectedIndex == 0) {
                selectedIndex = 1;
            } else if (selectedIndex == 1) {
                selectedIndex = 0;
            }
            updateButtonStyles();
            event.consume();
        } else if (code == KeyCode.RIGHT || code == KeyCode.D) {
            if (selectedIndex == 0) {
                selectedIndex = 1;
            } else if (selectedIndex == 1) {
                selectedIndex = 0;
            }
            updateButtonStyles();
            event.consume();
        } else if (code == KeyCode.ENTER || code == KeyCode.SPACE) {
            buttons[selectedIndex].fire();
            event.consume();
        } else if (code == KeyCode.ESCAPE) {
            if (onBack != null) {
                onBack.run();
            }
            event.consume();
        }
    }

    private void updateButtonStyles() {
        for (int i = 0; i < buttons.length; i++) {
            if (i == selectedIndex) {
                buttons[i].setStyle("-fx-background-color: rgba(250, 241, 70, 0.9); " +
                        "-fx-text-fill: black; " +
                        "-fx-background-radius: 5; " +
                        "-fx-border-color: rgba(255, 255, 255, 0.9); " +
                        "-fx-border-width: 3; " +
                        "-fx-border-radius: 5;");
            } else {
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
        setSelectedIndex(2); // start with Back button selected
    }
}