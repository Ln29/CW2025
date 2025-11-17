package com.comp2042.ui.menu;

import com.comp2042.config.KeyBindingsConfig;
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

public class GameOverMenu extends VBox {

    private Button restartButton;
    private Button mainMenuButton;
    private int selectedIndex = 0;
    private Button[] buttons;
    private Label titleLabel;

    public GameOverMenu() {
        setAlignment(Pos.CENTER);
        setSpacing(15);
        setPadding(new Insets(30, 50, 30, 50));
        setStyle("-fx-background-color: rgba(0, 0, 0, 0.8); -fx-background-radius: 10;");

        titleLabel = new Label("GAME OVER");
        titleLabel.setTextFill(Color.RED);
        titleLabel.setFont(Font.font("Arial", FontWeight.BOLD, 36));
        titleLabel.setPadding(new Insets(0, 0, 20, 0));

        restartButton = createButton("Restart");
        restartButton.setOnAction(e -> {
            if (onRestart != null) {
                onRestart.run();
            }
        });
        restartButton.setOnMouseEntered(e -> setSelectedIndex(0));

        mainMenuButton = createButton("Main Menu");
        mainMenuButton.setOnAction(e -> {
            if (onMainMenu != null) {
                onMainMenu.run();
            }
        });
        mainMenuButton.setOnMouseEntered(e -> setSelectedIndex(1));

        buttons = new Button[]{restartButton, mainMenuButton};
        updateButtonStyles();

        getChildren().addAll(titleLabel, restartButton, mainMenuButton);

        // Handle keyboard navigation
        setOnKeyPressed(this::handleKeyPress);
        setFocusTraversable(true);
        setPrefWidth(350);
        setPrefHeight(300);
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
        KeyBindingsConfig config = KeyBindingsConfig.getInstance();

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

    private Runnable onRestart;
    private Runnable onMainMenu;

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

    public void setWinState() {
        if (titleLabel != null) {
            titleLabel.setText("YOU WIN!");
            titleLabel.setTextFill(Color.GREEN);
        }
    }

    public void setGameOverState() {
        if (titleLabel != null) {
            titleLabel.setText("GAME OVER");
            titleLabel.setTextFill(Color.RED);
        }
    }
}

