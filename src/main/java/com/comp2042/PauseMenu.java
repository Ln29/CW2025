package com.comp2042;

import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;
import javafx.scene.text.Font;
import javafx.scene.text.FontWeight;

public class PauseMenu extends VBox {

    private Button resumeButton;
    private Button restartButton;
    private Button mainMenuButton;
    private int selectedIndex = 0;
    private Button[] buttons;

    public PauseMenu() {
        setAlignment(Pos.CENTER);
        setSpacing(15);
        setPadding(new Insets(30, 50, 30, 50));
        setStyle("-fx-background-color: rgba(0, 0, 0, 0.8); -fx-background-radius: 10;");

        // Title
        Label titleLabel = new Label("PAUSED");
        titleLabel.setTextFill(Color.RED);
        titleLabel.setFont(Font.font("Arial", FontWeight.BOLD, 36));
        titleLabel.setPadding(new Insets(0, 0, 20, 0));

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

        KeyBindingsConfig.Action action = config.getAction(code);
        boolean isUp = (code == KeyCode.UP) || (action == KeyBindingsConfig.Action.ROTATE);
        boolean isDown = (code == KeyCode.DOWN) || (action == KeyBindingsConfig.Action.SOFT_DROP);
        boolean isSelect = (code == KeyCode.ENTER || code == KeyCode.SPACE) || (action == KeyBindingsConfig.Action.HARD_DROP);
        boolean isBack = (code == KeyCode.ESCAPE) || (action == KeyBindingsConfig.Action.PAUSE);

        if (isUp) {
            selectedIndex = selectedIndex - 1;
            if (selectedIndex < 0) {
                selectedIndex = buttons.length - 1;
            }
            updateButtonStyles();
            event.consume();
        } else if (isDown) {
            selectedIndex = selectedIndex + 1;
            if (selectedIndex >= buttons.length) {
                selectedIndex = 0;
            }
            updateButtonStyles();
            event.consume();
        } else if (isSelect) {
            buttons[selectedIndex].fire();
            event.consume();
        } else if (isBack) {
            if (onResume != null) {
                onResume.run();
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