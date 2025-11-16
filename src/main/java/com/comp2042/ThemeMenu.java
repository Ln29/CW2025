package com.comp2042;

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
import javafx.scene.text.Font;
import javafx.scene.text.FontWeight;

import java.util.ArrayList;
import java.util.List;

public class ThemeMenu extends VBox {

    public enum Theme {
        DEFAULT("Default", "default.png", "default.wav", new String[]{"#00FFFF", "#8A2BE2", "#006400", "#FFFF00", "#FF0000", "#F5F5DC", "#DEB887"}),
        SNOWY_MOUNTAIN("Snowy Mountain", "snowy_mountain.png", "snowy_mountain.wav", new String[]{"#AEEEEE", "#F0F8FF", "#B0C4DE", "#98FB98", "#AFEEEE", "#4682B4", "#E0FFFF"}),
        BEACH("Beach", "beach.png", "beach.wav", new String[]{"#1E90FF", "#FFD700", "#FF7F50", "#32CD32", "#F0FFFF", "#FFB6C1", "#008B8B"}),
        PARTY("Party", "party.jpg", "party.wav", new String[]{"#DA70D6", "#FFFF00", "#FF69B4", "#7CFC00", "#FFA500", "#1E90FF", "#FF4500"});

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
        config = KeyBindingsConfig.getInstance();
        themeOptions = new ArrayList<>();

        setAlignment(Pos.CENTER);
        setSpacing(10);
        setPadding(new Insets(30, 60, 40, 60));
        setStyle("-fx-background-color: rgba(0, 0, 0, 0.9); -fx-background-radius: 10;");

        // Title
        Label titleLabel = new Label("THEMES");
        titleLabel.setTextFill(Color.RED);
        titleLabel.setFont(Font.font("Arial", FontWeight.BOLD, 36));
        titleLabel.setPadding(new Insets(0, 0, 0, 0));

        //create theme grid (2x2)
        themeGrid = new GridPane();
        themeGrid.setAlignment(Pos.CENTER);
        themeGrid.setHgap(20);
        themeGrid.setVgap(20);
        themeGrid.setPadding(new Insets(5));

        // Create theme options and add to grid
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

        // Handle keyboard navigation
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
        themeBox.setAlignment(Pos.CENTER);
        themeBox.setPadding(new Insets(15));
        themeBox.setStyle("-fx-background-color: rgba(50, 50, 50, 0.5); -fx-background-radius: 5;");

        // Theme name
        Label themeLabel = new Label(theme.getDisplayName());
        themeLabel.setTextFill(Color.WHITE);
        themeLabel.setFont(Font.font("Arial", FontWeight.BOLD, 20));

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

        // Select button
        Button selectButton = createButton("Select");
        selectButton.setPrefWidth(150);
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

        // Check if pressed key match bound action
        KeyBindingsConfig.Action action = config.getAction(code);
        boolean isUp = (code == KeyCode.UP) || (action == KeyBindingsConfig.Action.ROTATE);
        boolean isDown = (code == KeyCode.DOWN) || (action == KeyBindingsConfig.Action.SOFT_DROP);
        boolean isLeft = (code == KeyCode.LEFT) || (action == KeyBindingsConfig.Action.MOVE_LEFT);
        boolean isRight = (code == KeyCode.RIGHT) || (action == KeyBindingsConfig.Action.MOVE_RIGHT);
        boolean isSelect = (code == KeyCode.ENTER || code == KeyCode.SPACE) || (action == KeyBindingsConfig.Action.HARD_DROP);
        boolean isBack = (code == KeyCode.ESCAPE) || (action == KeyBindingsConfig.Action.PAUSE);

        // If back button is selected, only allow up/down to go back to grid
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
        setSelectedIndex(themeOptions.size()); // Start with Back button selected
    }

    private static class ThemeOption {
        Theme theme;
        Button selectButton;
        VBox container;
    }

    @FunctionalInterface
    public interface ThemeSelectionCallback {
        void run(Theme theme);
    }
}