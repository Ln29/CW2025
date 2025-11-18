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
        getStylesheets().add(getClass().getResource("/menu_style.css").toExternalForm());
        getStyleClass().add("menu-container");
        setAlignment(Pos.CENTER);
        setSpacing(20);
        setPadding(new Insets(40, 60, 40, 60));

        Label titleLabel = new Label("SETTINGS");
        titleLabel.getStyleClass().add("menu-title-red");

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

        HBox bottomButtonsBox = new HBox(20);
        bottomButtonsBox.setAlignment(Pos.CENTER);
        bottomButtonsBox.setPadding(new Insets(10, 0, 10, 0));

        // Key Bindings button (left)
        keyBindingsButton = createButton("Key Bindings");
        keyBindingsButton.setOnAction(e -> {
            if (onKeyBindings != null) {
                onKeyBindings.run();
            }
        });
        keyBindingsButton.setOnMouseEntered(e -> setSelectedIndex(0));

        // Theme Selection button (right)
        themeSelectionButton = createButton("Themes");
        themeSelectionButton.setOnAction(e -> {
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
        label.getStyleClass().add("menu-label-large");
        label.setPrefWidth(180);

        Slider slider = new Slider(min, max, defaultValue);
        slider.setPrefWidth(150);
        slider.setShowTickLabels(false);
        slider.setShowTickMarks(false);

        Label valueLabel = new Label(String.valueOf((int) defaultValue));
        valueLabel.getStyleClass().add("menu-label-large");
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
        button.getStyleClass().add("menu-button");
        return button;
    }

    private void handleKeyPress(KeyEvent event) {
        KeyCode code = event.getCode();
        KeyBindingsConfig config = KeyBindingsConfig.getInstance();

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
            if (selectedIndex == 0) {
                selectedIndex = 1;
            } else if (selectedIndex == 1) {
                selectedIndex = 0;
            }
            updateButtonStyles();
            event.consume();
        } else if (isRight) {
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
                if (!buttons[i].getStyleClass().contains("selected")) {
                    buttons[i].getStyleClass().add("selected");
                }
            } else {
                buttons[i].getStyleClass().remove("selected");
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
        setSelectedIndex(2);
    }
}

