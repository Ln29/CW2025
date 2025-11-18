package com.comp2042.ui.menu;

import com.comp2042.config.KeyBindingsConfig;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;
import javafx.scene.text.Font;
import javafx.scene.text.FontWeight;

import java.util.ArrayList;
import java.util.List;

public class KeyBindingsMenu extends VBox {

    private Button backButton;
    private int selectedIndex = 0;
    private Button[] buttons;
    private List<KeyBindingRow> bindingRows;
    private KeyBindingsConfig config;
    private KeyBindingRow currentlyRebinding;

    public KeyBindingsMenu() {
        getStylesheets().add(getClass().getResource("/menu_style.css").toExternalForm());
        getStyleClass().add("menu-container");
        config = KeyBindingsConfig.getInstance();
        bindingRows = new ArrayList<>();

        setAlignment(Pos.CENTER);
        setSpacing(20);
        setPadding(new Insets(40, 60, 40, 60));

        Label titleLabel = new Label("KEY BINDINGS");
        titleLabel.getStyleClass().add("menu-title-red");

        // Create key binding rows
        createKeyBindingRow("Move Left", KeyBindingsConfig.Action.MOVE_LEFT);
        createKeyBindingRow("Move Right", KeyBindingsConfig.Action.MOVE_RIGHT);
        createKeyBindingRow("Rotate", KeyBindingsConfig.Action.ROTATE);
        createKeyBindingRow("Soft Drop", KeyBindingsConfig.Action.SOFT_DROP);
        createKeyBindingRow("Hard Drop", KeyBindingsConfig.Action.HARD_DROP);
        createKeyBindingRow("Hold", KeyBindingsConfig.Action.HOLD);
        createKeyBindingRow("Pause", KeyBindingsConfig.Action.PAUSE);

        // Back button
        backButton = createButton("Back");
        backButton.setOnAction(e -> {
            if (onBack != null) {
                onBack.run();
            }
        });
        backButton.setOnMouseEntered(e -> setSelectedIndex(bindingRows.size()));

        buttons = new Button[bindingRows.size() + 1];
        for (int i = 0; i < bindingRows.size(); i++) {
            buttons[i] = bindingRows.get(i).rebindButton;
        }
        buttons[bindingRows.size()] = backButton;

        updateButtonStyles();

        // Add all content directly to this VBox
        getChildren().add(titleLabel);
        for (KeyBindingRow row : bindingRows) {
            getChildren().add(row.container);
        }
        getChildren().add(backButton);

        setOnKeyPressed(this::handleKeyPress);
        setFocusTraversable(true);
        setPrefWidth(600);
        setPrefHeight(700);
        setViewOrder(-1);
    }

    private void createKeyBindingRow(String actionName, KeyBindingsConfig.Action action) {
        KeyBindingRow row = new KeyBindingRow();
        row.action = action;

        HBox hbox = new HBox(80);
        hbox.setAlignment(Pos.CENTER_LEFT);
        hbox.setPrefWidth(400);

        Label actionLabel = new Label(actionName + ":");
        actionLabel.getStyleClass().add("menu-label-large");
        actionLabel.setPrefWidth(150);

        Label keyLabel = new Label(getKeyName(config.getKey(action)));
        keyLabel.setTextFill(Color.YELLOW);
        keyLabel.setFont(Font.font("Arial", FontWeight.BOLD, 16));
        keyLabel.setPrefWidth(120);
        keyLabel.setAlignment(Pos.CENTER_LEFT);
        keyLabel.setPadding(new Insets(0, 0, 0, -20));

        Button rebindButton = createButton("Rebind");
        rebindButton.getStyleClass().remove("menu-button");
        rebindButton.getStyleClass().add("action-button");
        rebindButton.setPrefWidth(120);
        rebindButton.setOnAction(e -> startRebinding(row));
        final int rowIndex = bindingRows.size();
        rebindButton.setOnMouseEntered(e -> setSelectedIndex(rowIndex));

        row.actionLabel = actionLabel;
        row.keyLabel = keyLabel;
        row.rebindButton = rebindButton;
        row.container = hbox;

        hbox.getChildren().addAll(actionLabel, keyLabel, rebindButton);
        bindingRows.add(row);
    }

    private void startRebinding(KeyBindingRow row) {
        if (currentlyRebinding != null) {
            // Cancel previous rebinding
            currentlyRebinding.keyLabel.setTextFill(Color.YELLOW);
            currentlyRebinding.keyLabel.setText(getKeyName(config.getKey(currentlyRebinding.action)));
        }

        currentlyRebinding = row;
        row.keyLabel.setTextFill(Color.RED);
        row.keyLabel.setText("Press a key...");
        row.rebindButton.setText("Cancel");
    }

    private void cancelRebinding() {
        if (currentlyRebinding != null) {
            currentlyRebinding.keyLabel.setTextFill(Color.YELLOW);
            currentlyRebinding.keyLabel.setText(getKeyName(config.getKey(currentlyRebinding.action)));
            currentlyRebinding.rebindButton.setText("Rebind");
            currentlyRebinding = null;
        }
    }

    private void finishRebinding(KeyCode newKey) {
        if (currentlyRebinding == null) return;

        KeyBindingsConfig.Action action = currentlyRebinding.action;

        // Check for conflicts
        if (config.hasConflict(newKey, action)) {
            currentlyRebinding.keyLabel.setTextFill(Color.RED);
            currentlyRebinding.keyLabel.setText("CONFLICT!");
            // Show conflict message briefly, then revert
            javafx.application.Platform.runLater(() -> {
                try {
                    Thread.sleep(1000);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
                javafx.application.Platform.runLater(() -> {
                    currentlyRebinding.keyLabel.setTextFill(Color.YELLOW);
                    currentlyRebinding.keyLabel.setText(getKeyName(config.getKey(action)));
                });
            });
            currentlyRebinding.rebindButton.setText("Rebind");
            currentlyRebinding = null;
            return;
        }

        // Set the new key binding
        config.setKey(action, newKey);
        currentlyRebinding.keyLabel.setTextFill(Color.YELLOW);
        currentlyRebinding.keyLabel.setText(getKeyName(newKey));
        currentlyRebinding.rebindButton.setText("Rebind");
        currentlyRebinding = null;

        // Notify that bindings have changed
        if (onBindingsChanged != null) {
            onBindingsChanged.run();
        }
    }

    private String getKeyName(KeyCode key) {
        if (key == null) return "NONE";
        String name = key.getName();
        if (name.length() > 1) {
            return name.charAt(0) + name.substring(1).toLowerCase();
        }
        return name;
    }

    private Button createButton(String text) {
        Button button = new Button(text);
        button.getStyleClass().add("menu-button");
        return button;
    }

    private void handleKeyPress(KeyEvent event) {
        KeyCode code = event.getCode();

        // If rebinding, handle the key press for rebinding
        if (currentlyRebinding != null) {
            KeyBindingsConfig.Action action = config.getAction(code);
            boolean isBack = (code == KeyCode.ESCAPE) || (action == KeyBindingsConfig.Action.PAUSE);

            if (isBack) {
                cancelRebinding();
                event.consume();
                return;
            }

            boolean isUp = (code == KeyCode.UP) || (action == KeyBindingsConfig.Action.ROTATE);
            boolean isDown = (code == KeyCode.DOWN) || (action == KeyBindingsConfig.Action.SOFT_DROP);
            boolean isSelect = (code == KeyCode.ENTER || code == KeyCode.SPACE) || (action == KeyBindingsConfig.Action.HARD_DROP);

            if (!isUp && !isDown && !isSelect) {
                finishRebinding(code);
                event.consume();
                return;
            }
        }

        KeyBindingsConfig.Action action = config.getAction(code);
        boolean isUp = (code == KeyCode.UP) || (action == KeyBindingsConfig.Action.ROTATE);
        boolean isDown = (code == KeyCode.DOWN) || (action == KeyBindingsConfig.Action.SOFT_DROP);
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

    public void refreshBindings() {
        for (KeyBindingRow row : bindingRows) {
            row.keyLabel.setText(getKeyName(config.getKey(row.action)));
        }
    }

    // Callbacks
    private Runnable onBack;
    private Runnable onBindingsChanged;

    public void setOnBack(Runnable onBack) {
        this.onBack = onBack;
    }

    public void setOnBindingsChanged(Runnable onBindingsChanged) {
        this.onBindingsChanged = onBindingsChanged;
    }

    public void requestFocusForNavigation() {
        requestFocus();
        setSelectedIndex(bindingRows.size());
    }

    // Inner class to hold key binding row data
    private static class KeyBindingRow {
        KeyBindingsConfig.Action action;
        @SuppressWarnings("unused")
        Label actionLabel;
        Label keyLabel;
        Button rebindButton;
        HBox container;
    }
}