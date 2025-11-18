package com.comp2042.ui.menu;

import com.comp2042.config.KeyBindingsConfig;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;
import javafx.scene.shape.Rectangle;

import java.util.ArrayList;
import java.util.List;

public class ThemeMenu extends VBox {

    public enum Theme {
        DEFAULT("Default", "default.png", "default.mp3",
                new String[]{"#FF6B6B", "#FFA500", "#FFD93D", "#6BCF7F", "#4ECDC4", "#A78BFA", "#FF6BA9"}),
        SNOWY_MOUNTAIN("Snowy Mountain", "snowy_mountain.png", "snowy_mountain.wav",
                new String[]{"#90C9E8", "#AAD5E8", "#95D9B8", "#B8E6D0", "#C9A8D8", "#E5D4C1", "#F0F8FF"}),
        BEACH("Beach", "beach.png", "beach.wav",
                new String[]{"#5DDEF4", "#7FE7FF", "#FFF176", "#FFDB58", "#FFBC42", "#E8C68E", "#B0E0E6"}),
        PARTY("Party", "party.jpg", "party.wav",
                new String[]{"#FF1493", "#FF00FF", "#FF69B4", "#00FFFF", "#00FF00", "#FFFF00", "#FF4500"});

        private final String displayName;
        private final String imagePath;
        private final String musicFile;
        private final String[] brickColors;

        Theme(String displayName, String imagePath, String musicFile, String[] brickColors) {
            this.displayName = displayName;
            this.imagePath = imagePath;
            this.musicFile = musicFile;
            this.brickColors = brickColors;
        }

        public String getDisplayName() {
            return displayName;
        }

        public String getImagePath() {
            return imagePath;
        }

        public String getMusicFile() {
            return musicFile;
        }

        public String[] getBrickColors() {
            return brickColors;
        }
    }

    private Button backButton;
    private int selectedIndex = 0;
    private int selectedRow = 0;
    private int selectedCol = 0;
    private Button[] buttons;
    private List<ThemeOption> themeOptions;
    private KeyBindingsConfig config;
    private Theme selectedTheme = Theme.DEFAULT;
    private GridPane themeGrid;

    public ThemeMenu() {
        getStylesheets().add(getClass().getResource("/menu_style.css").toExternalForm());
        getStyleClass().add("menu-container");
        config = KeyBindingsConfig.getInstance();
        themeOptions = new ArrayList<>();

        setAlignment(Pos.CENTER);
        setSpacing(10);
        setPadding(new Insets(30, 60, 40, 60));

        Label titleLabel = new Label("THEMES");
        titleLabel.getStyleClass().add("menu-title-red");

        // Create theme grid (2x2)
        themeGrid = new GridPane();
        themeGrid.setAlignment(Pos.CENTER);
        themeGrid.setHgap(20);
        themeGrid.setVgap(20);
        themeGrid.setPadding(new Insets(5));

        Theme[] themes = Theme.values();
        for (int i = 0; i < themes.length; i++) {
            createThemeOption(themes[i]);
            int row = i / 2;
            int col = i % 2;
            themeGrid.add(themeOptions.get(i).container, col, row);
        }

        // Back button
        backButton = createButton("Back");
        backButton.setOnAction(e -> {
            if (onBack != null) {
                onBack.run();
            }
        });
        backButton.setOnMouseEntered(e -> {
            selectedIndex = themeOptions.size();
            updateButtonStyles();
        });

        buttons = new Button[themeOptions.size() + 1];
        for (int i = 0; i < themeOptions.size(); i++) {
            buttons[i] = themeOptions.get(i).selectButton;
        }
        buttons[themeOptions.size()] = backButton;

        updateButtonStyles();

        // Add all content directly to this VBox
        getChildren().add(titleLabel);
        getChildren().add(themeGrid);
        getChildren().add(backButton);

        setOnKeyPressed(this::handleKeyPress);
        setFocusTraversable(true);
        setPrefWidth(600);
        setPrefHeight(700);
        setViewOrder(-1);
    }

    private void createThemeOption(Theme theme) {
        ThemeOption option = new ThemeOption();
        option.theme = theme;

        VBox themeBox = new VBox(10);
        themeBox.getStyleClass().add("theme-box");
        themeBox.setAlignment(Pos.CENTER);
        themeBox.setPadding(new Insets(15));

        Label themeLabel = new Label(theme.getDisplayName());
        themeLabel.getStyleClass().add("menu-label-large");

        // Preview image
        ImageView imageView = new ImageView();
        try {
            Image image = new Image(getClass().getClassLoader().getResource("assets/images/" + theme.getImagePath()).toExternalForm());
            imageView.setImage(image);
            imageView.setFitWidth(200);
            imageView.setFitHeight(120);
            imageView.setPreserveRatio(true);
        } catch (Exception e) {
            System.err.println("Error loading theme image: " + e.getMessage());
        }

        // Brick colors preview
        HBox colorBox = new HBox(5);
        colorBox.setAlignment(Pos.CENTER);
        String[] colors = theme.getBrickColors();
        for (int i = 0; i < colors.length; i++) {
            VBox colorPreview = new VBox(3);
            colorPreview.setAlignment(Pos.CENTER);

            Rectangle colorRect = new Rectangle(25, 25);
            try {
                colorRect.setFill(Color.web(colors[i]));
            } catch (Exception e) {
                colorRect.setFill(Color.GRAY);
            }
            colorRect.setStroke(Color.WHITE);
            colorRect.setStrokeWidth(1);

            colorPreview.getChildren().addAll(colorRect);
            colorBox.getChildren().add(colorPreview);
        }

        Button selectButton = createButton("Select");
        selectButton.getStyleClass().remove("menu-button");
        selectButton.getStyleClass().add("action-button");
        selectButton.setOnAction(e -> selectTheme(theme));
        final int optionIndex = themeOptions.size();
        selectButton.setOnMouseEntered(e -> {
            selectedIndex = optionIndex;
            selectedRow = optionIndex / 2;
            selectedCol = optionIndex % 2;
            updateButtonStyles();
        });

        option.selectButton = selectButton;
        option.container = themeBox;

        themeBox.getChildren().addAll(themeLabel, imageView, colorBox, selectButton);
        themeOptions.add(option);
    }

    private void selectTheme(Theme theme) {
        selectedTheme = theme;
        if (onThemeSelected != null) {
            onThemeSelected.run(theme);
        }
        // Update button text to show selected
        for (ThemeOption option : themeOptions) {
            if (option.theme == theme) {
                option.selectButton.setText("Selected");
            } else {
                option.selectButton.setText("Select");
            }
        }
    }

    private Button createButton(String text) {
        Button button = new Button(text);
        button.getStyleClass().add("menu-button");
        return button;
    }

    private void handleKeyPress(KeyEvent event) {
        KeyCode code = event.getCode();

        KeyBindingsConfig.Action action = config.getAction(code);
        boolean isUp = (code == KeyCode.UP) || (action == KeyBindingsConfig.Action.ROTATE);
        boolean isDown = (code == KeyCode.DOWN) || (action == KeyBindingsConfig.Action.SOFT_DROP);
        boolean isLeft = (code == KeyCode.LEFT) || (action == KeyBindingsConfig.Action.MOVE_LEFT);
        boolean isRight = (code == KeyCode.RIGHT) || (action == KeyBindingsConfig.Action.MOVE_RIGHT);
        boolean isSelect = (code == KeyCode.ENTER || code == KeyCode.SPACE) || (action == KeyBindingsConfig.Action.HARD_DROP);
        boolean isBack = (code == KeyCode.ESCAPE) || (action == KeyBindingsConfig.Action.PAUSE);

        if (selectedIndex == themeOptions.size()) {
            if (isUp) {
                selectedIndex = themeOptions.size() - 1;
                selectedRow = (themeOptions.size() - 1) / 2;
                selectedCol = (themeOptions.size() - 1) % 2;
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
            return;
        }

        if (isUp) {
            if (selectedRow > 0) {
                selectedRow--;
            } else {
                selectedIndex = themeOptions.size();
                updateButtonStyles();
                event.consume();
                return;
            }
            selectedIndex = selectedRow * 2 + selectedCol;
            updateButtonStyles();
            event.consume();
        } else if (isDown) {
            if (selectedRow < 1) {
                selectedRow++;
            }
            selectedIndex = selectedRow * 2 + selectedCol;
            updateButtonStyles();
            event.consume();
        } else if (isLeft) {
            if (selectedCol > 0) {
                selectedCol--;
            }
            selectedIndex = selectedRow * 2 + selectedCol;
            updateButtonStyles();
            event.consume();
        } else if (isRight) {
            if (selectedCol < 1) {
                selectedCol++;
            }
            selectedIndex = selectedRow * 2 + selectedCol;
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
            if (index < themeOptions.size()) {
                selectedRow = index / 2;
                selectedCol = index % 2;
            }
            updateButtonStyles();
        }
    }

    public int getSelectedIndex() {
        return selectedIndex;
    }

    public Theme getSelectedTheme() {
        return selectedTheme;
    }

    private Runnable onBack;
    private ThemeSelectionCallback onThemeSelected;

    public void setOnBack(Runnable onBack) {
        this.onBack = onBack;
    }

    public void setOnThemeSelected(ThemeSelectionCallback onThemeSelected) {
        this.onThemeSelected = onThemeSelected;
    }

    public void requestFocusForNavigation() {
        requestFocus();
        setSelectedIndex(themeOptions.size());
    }

    // Inner class to hold theme option data
    private static class ThemeOption {
        Theme theme;
        Button selectButton;
        VBox container;
    }

    // Callback interface for theme selection
    @FunctionalInterface
    public interface ThemeSelectionCallback {
        void run(Theme theme);
    }
}