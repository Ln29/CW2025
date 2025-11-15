package com.comp2042;

import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
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
            setStyle("-fx-background-color: rgba(0, 0, 0, 0.8);");
        }

        setAlignment(Pos.CENTER);
        setSpacing(15);
        setPadding(new Insets(30, 50, 30, 50));
        setPrefWidth(900);
        setPrefHeight(700);

        setViewOrder(-1);

        Label titleLabel = new Label("TETRIS");
        titleLabel.setTextFill(Color.PURPLE);
        titleLabel.setFont(Font.font("Arial", FontWeight.BOLD, 48));
        titleLabel.setPadding(new Insets(0, 0, 40, 0));

        startButton = createButton("Start");
        startButton.setOnAction(e -> {
            if (onStart != null) {
                onStart.run();
            }
        });
        startButton.setOnMouseEntered(e -> setSelectedIndex(0));

        settingsButton = createButton("Settings");
        settingsButton.setOnAction(e -> {
            // TO DO
            if (onSettings != null) {
                onSettings.run();
            }
        });
        settingsButton.setOnMouseEntered(e -> setSelectedIndex(1));

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
        } else if (code == KeyCode.ENTER || code == KeyCode.SPACE) {
            buttons[selectedIndex].fire();
            event.consume();
        }
    }

    private void updateButtonStyles() {
        for (int i = 0; i < buttons.length; i++) {
            if (i == selectedIndex) {
                buttons[i].setStyle("-fx-background-color: rgba(255, 215, 0, 0.9); " +
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