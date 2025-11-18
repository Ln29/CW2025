package com.comp2042.ui.menu;

import com.comp2042.config.GameModeConfig;
import com.comp2042.core.GameMode;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.control.*;
import javafx.scene.layout.*;
import javafx.scene.paint.Color;
import javafx.scene.shape.Rectangle;
import javafx.scene.text.Font;
import javafx.scene.text.FontWeight;

public class ModeSelectionMenu extends VBox {

    private GameMode selectedMode = GameMode.ENDLESS;
    private int difficulty = 5;
    private int marathonTargetLines = 100;
    private GameModeConfig.SurvivalDifficulty survivalDifficulty = GameModeConfig.SurvivalDifficulty.SIMPLE;

    private VBox modeOptionsContainer;
    private Button startButton;
    private Button backButton;

    // Callbacks
    private Runnable onStart;
    private Runnable onBack;

    public ModeSelectionMenu() {
        setAlignment(Pos.CENTER);
        setSpacing(20);
        setPadding(new Insets(30, 50, 30, 50));
        setPrefWidth(600);
        setPrefHeight(700);
        setStyle("-fx-background-color: rgba(0, 0, 0, 0.9);");
        setViewOrder(-1);

        // Title
        Label titleLabel = new Label("SELECT GAME MODE");
        titleLabel.setTextFill(Color.YELLOW);
        titleLabel.setFont(Font.font("Arial", FontWeight.BOLD, 36));
        titleLabel.setPadding(new Insets(0, 0, 20, 0));

        // Mode selection buttons
        HBox modeButtons = new HBox(15);
        modeButtons.setAlignment(Pos.CENTER);

        Button endlessButton = createModeButton("ENDLESS", GameMode.ENDLESS);
        Button marathonButton = createModeButton("MARATHON", GameMode.MARATHON);
        Button survivalButton = createModeButton("SURVIVAL", GameMode.SURVIVAL);

        modeButtons.getChildren().addAll(endlessButton, marathonButton, survivalButton);

        // Mode-specific options container
        modeOptionsContainer = new VBox(15);
        modeOptionsContainer.setAlignment(Pos.CENTER);
        modeOptionsContainer.setPadding(new Insets(20, 0, 20, 0));

        // Action buttons
        HBox actionButtons = new HBox(20);
        actionButtons.setAlignment(Pos.CENTER);

        startButton = createActionButton("START");
        startButton.setOnAction(e -> {
            if (onStart != null) {
                onStart.run();
            }
        });

        backButton = createActionButton("BACK");
        backButton.setOnAction(e -> {
            if (onBack != null) {
                onBack.run();
            }
        });

        actionButtons.getChildren().addAll(startButton, backButton);

        getChildren().addAll(titleLabel, modeButtons, modeOptionsContainer, actionButtons);

        updateModeOptions();
    }

    private Button createModeButton(String text, GameMode mode) {
        Button button = new Button(text);
        button.setPrefWidth(200);
        button.setPrefHeight(50);
        button.setFont(Font.font("Arial", FontWeight.BOLD, 16));
        button.setStyle("-fx-background-color: rgba(100, 100, 100, 0.7); " +
                "-fx-text-fill: white; " +
                "-fx-background-radius: 5; " +
                "-fx-border-color: rgba(200, 200, 200, 0.5); " +
                "-fx-border-width: 2; " +
                "-fx-border-radius: 5;");

        button.setOnAction(e -> {
            selectedMode = mode;
            updateModeOptions();
            updateModeButtonStyles(button);
        });

        button.setOnMouseEntered(e -> {
            if (selectedMode != mode) {
                button.setStyle("-fx-background-color: rgba(120, 120, 120, 0.8); " +
                        "-fx-text-fill: white; " +
                        "-fx-background-radius: 5; " +
                        "-fx-border-color: rgba(200, 200, 200, 0.7); " +
                        "-fx-border-width: 2; " +
                        "-fx-border-radius: 5;");
            }
        });

        button.setOnMouseExited(e -> {
            if (selectedMode != mode) {
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

    private void updateModeButtonStyles(Button selectedButton) {
        for (var child : ((HBox) getChildren().get(1)).getChildren()) {
            if (child instanceof Button) {
                Button btn = (Button) child;
                if (btn == selectedButton) {
                    btn.setStyle("-fx-background-color: rgba(255, 215, 0, 0.9); " +
                            "-fx-text-fill: black; " +
                            "-fx-background-radius: 5; " +
                            "-fx-border-color: rgba(255, 255, 255, 0.9); " +
                            "-fx-border-width: 3; " +
                            "-fx-border-radius: 5;");
                } else {
                    btn.setStyle("-fx-background-color: rgba(100, 100, 100, 0.7); " +
                            "-fx-text-fill: white; " +
                            "-fx-background-radius: 5; " +
                            "-fx-border-color: rgba(200, 200, 200, 0.5); " +
                            "-fx-border-width: 2; " +
                            "-fx-border-radius: 5;");
                }
            }
        }
    }

    private Button createActionButton(String text) {
        Button button = new Button(text);
        button.setPrefWidth(150);
        button.setPrefHeight(45);
        button.setFont(Font.font("Arial", FontWeight.BOLD, 18));
        button.setStyle("-fx-background-color: rgba(100, 100, 100, 0.7); " +
                "-fx-text-fill: white; " +
                "-fx-background-radius: 5; " +
                "-fx-border-color: rgba(200, 200, 200, 0.5); " +
                "-fx-border-width: 2; " +
                "-fx-border-radius: 5;");

        button.setOnMouseEntered(e -> {
            button.setStyle("-fx-background-color: rgba(120, 120, 120, 0.8); " +
                    "-fx-text-fill: white; " +
                    "-fx-background-radius: 5; " +
                    "-fx-border-color: rgba(200, 200, 200, 0.7); " +
                    "-fx-border-width: 2; " +
                    "-fx-border-radius: 5;");
        });

        button.setOnMouseExited(e -> {
            button.setStyle("-fx-background-color: rgba(100, 100, 100, 0.7); " +
                    "-fx-text-fill: white; " +
                    "-fx-background-radius: 5; " +
                    "-fx-border-color: rgba(200, 200, 200, 0.5); " +
                    "-fx-border-width: 2; " +
                    "-fx-border-radius: 5;");
        });

        return button;
    }

    private void updateModeOptions() {
        modeOptionsContainer.getChildren().clear();

        switch (selectedMode) {
            case ENDLESS:
                addEndlessOptions();
                break;
            case MARATHON:
                addMarathonOptions();
                break;
            case SURVIVAL:
                addSurvivalOptions();
                break;
        }
    }

    private void addEndlessOptions() {
        Label description = new Label("Goal: Clear 999 lines\nFixed speed throughout");
        description.setTextFill(Color.WHITE);
        description.setFont(Font.font("Arial", 14));
        description.setAlignment(Pos.CENTER);

        Label difficultyLabel = new Label("Difficulty (0-10): " + difficulty);
        difficultyLabel.setTextFill(Color.WHITE);
        difficultyLabel.setFont(Font.font("Arial", 14));

        HBox difficultySelector = createVolumeBarDifficultySelector(difficultyLabel);

        modeOptionsContainer.getChildren().addAll(description, difficultyLabel, difficultySelector);
    }

    private void addMarathonOptions() {
        Label description = new Label("Target lines with increasing speed");
        description.setTextFill(Color.WHITE);
        description.setFont(Font.font("Arial", 14));
        description.setAlignment(Pos.CENTER);

        Label targetLabel = new Label("Target Lines: " + marathonTargetLines);
        targetLabel.setTextFill(Color.WHITE);
        targetLabel.setFont(Font.font("Arial", 14));

        HBox targetButtons = new HBox(10);
        targetButtons.setAlignment(Pos.CENTER);

        for (int target : new int[]{50, 100, 200, 500}) {
            Button btn = new Button(String.valueOf(target));
            btn.setPrefWidth(80);
            btn.setPrefHeight(35);
            btn.setFont(Font.font("Arial", 12));
            btn.setStyle(marathonTargetLines == target ?
                    "-fx-background-color: rgba(255, 215, 0, 0.9); -fx-text-fill: black;" :
                    "-fx-background-color: rgba(100, 100, 100, 0.7); -fx-text-fill: white;");

            int finalTarget = target;
            btn.setOnAction(e -> {
                marathonTargetLines = finalTarget;
                targetLabel.setText("Target Lines: " + marathonTargetLines);
                updateTargetButtonStyles(targetButtons, btn);
            });

            targetButtons.getChildren().add(btn);
        }

        Label difficultyLabel = new Label("Starting Difficulty (0-10): " + difficulty);
        difficultyLabel.setTextFill(Color.WHITE);
        difficultyLabel.setFont(Font.font("Arial", 14));

        HBox difficultySelector = createVolumeBarDifficultySelector(difficultyLabel);

        modeOptionsContainer.getChildren().addAll(description, targetLabel, targetButtons, difficultyLabel, difficultySelector);
    }

    private void updateTargetButtonStyles(HBox container, Button selected) {
        for (var child : container.getChildren()) {
            if (child instanceof Button) {
                Button btn = (Button) child;
                if (btn == selected) {
                    btn.setStyle("-fx-background-color: rgba(255, 215, 0, 0.9); -fx-text-fill: black;");
                } else {
                    btn.setStyle("-fx-background-color: rgba(100, 100, 100, 0.7); -fx-text-fill: white;");
                }
            }
        }
    }

    private void addSurvivalOptions() {
        // Description label
        Label description = new Label();
        description.setTextFill(Color.WHITE);
        description.setFont(Font.font("Arial", 14));
        description.setAlignment(Pos.CENTER);

        // Details label that will be updated
        Label details = new Label();
        details.setTextFill(Color.LIGHTGRAY);
        details.setFont(Font.font("Arial", 12));

        // Update description and details based on current difficulty
        Runnable updateDescription = () -> {
            String descText = String.format(
                    "Auto-spawn garbage rows from bottom\nTarget: %d lines | Speed: %dms",
                    survivalDifficulty.getTargetLines(),
                    survivalDifficulty.getStartSpeedMs()
            );
            description.setText(descText);

            String detailsText = String.format(
                    "Spawn Interval: Every %ds",
                    survivalDifficulty.getSpawnIntervalSeconds()
            );
            details.setText(detailsText);
        };

        updateDescription.run();

        Label difficultyLabel = new Label("Difficulty: " + survivalDifficulty.name());
        difficultyLabel.setTextFill(Color.WHITE);
        difficultyLabel.setFont(Font.font("Arial", 14));

        HBox difficultyButtons = new HBox(10);
        difficultyButtons.setAlignment(Pos.CENTER);

        for (GameModeConfig.SurvivalDifficulty diff : GameModeConfig.SurvivalDifficulty.values()) {
            Button btn = new Button(diff.name());
            btn.setPrefWidth(120);
            btn.setPrefHeight(35);
            btn.setFont(Font.font("Arial", 12));
            btn.setStyle(survivalDifficulty == diff ?
                    "-fx-background-color: rgba(255, 215, 0, 0.9); -fx-text-fill: black;" :
                    "-fx-background-color: rgba(100, 100, 100, 0.7); -fx-text-fill: white;");

            GameModeConfig.SurvivalDifficulty finalDiff = diff;
            btn.setOnAction(e -> {
                survivalDifficulty = finalDiff;
                difficultyLabel.setText("Difficulty: " + survivalDifficulty.name());
                updateDescription.run();
                updateSurvivalDifficultyButtonStyles(difficultyButtons, btn);
            });

            difficultyButtons.getChildren().add(btn);
        }

        modeOptionsContainer.getChildren().addAll(description, difficultyLabel, difficultyButtons, details);
    }

    private void updateSurvivalDifficultyButtonStyles(HBox container, Button selected) {
        for (var child : container.getChildren()) {
            if (child instanceof Button) {
                Button btn = (Button) child;
                if (btn == selected) {
                    btn.setStyle("-fx-background-color: rgba(255, 215, 0, 0.9); -fx-text-fill: black;");
                } else {
                    btn.setStyle("-fx-background-color: rgba(100, 100, 100, 0.7); -fx-text-fill: white;");
                }
            }
        }
    }

    public GameMode getSelectedMode() {
        return selectedMode;
    }

    public int getDifficulty() {
        return difficulty;
    }

    public int getMarathonTargetLines() {
        return marathonTargetLines;
    }

    public GameModeConfig.SurvivalDifficulty getSurvivalDifficulty() {
        return survivalDifficulty;
    }

    public void setOnStart(Runnable onStart) {
        this.onStart = onStart;
    }

    public void setOnBack(Runnable onBack) {
        this.onBack = onBack;
    }

    /**
     * Creates a volume bar style difficulty selector with arrow buttons and vertical bars
     * Layout: < [10 vertical bars] >
     */
    private HBox createVolumeBarDifficultySelector(Label difficultyLabel) {
        HBox container = new HBox(10);
        container.setAlignment(Pos.CENTER);

        // Left arrow button(decrease)
        Button leftArrow = new Button("<");
        leftArrow.setPrefWidth(30);
        leftArrow.setPrefHeight(50);
        leftArrow.setFont(Font.font("Arial", FontWeight.BOLD, 20));
        leftArrow.setStyle("-fx-background-color: rgba(100, 100, 100, 0.7); " +
                "-fx-text-fill: white; " +
                "-fx-background-radius: 5; " +
                "-fx-border-color: rgba(200, 200, 200, 0.5); " +
                "-fx-border-width: 2; " +
                "-fx-border-radius: 5;");

        leftArrow.setOnMouseEntered(e -> {
            leftArrow.setStyle("-fx-background-color: rgba(120, 120, 120, 0.8); " +
                    "-fx-text-fill: white; " +
                    "-fx-background-radius: 5; " +
                    "-fx-border-color: rgba(200, 200, 200, 0.7); " +
                    "-fx-border-width: 2; " +
                    "-fx-border-radius: 5;");
        });

        leftArrow.setOnMouseExited(e -> {
            leftArrow.setStyle("-fx-background-color: rgba(100, 100, 100, 0.7); " +
                    "-fx-text-fill: white; " +
                    "-fx-background-radius: 5; " +
                    "-fx-border-color: rgba(200, 200, 200, 0.5); " +
                    "-fx-border-width: 2; " +
                    "-fx-border-radius: 5;");
        });

        HBox barsContainer = new HBox(4);
        barsContainer.setAlignment(Pos.CENTER);
        barsContainer.setPadding(new Insets(0, 10, 0, 10));

        // Difficulty 0 = all grey
        Rectangle[] bars = new Rectangle[10];
        for (int i = 0; i < 10; i++) {
            Rectangle bar = new Rectangle(12, 50);
            bar.setArcWidth(3);
            bar.setArcHeight(3);
            bars[i] = bar;
            barsContainer.getChildren().add(bar);
        }

        // Right arrow button(increase)
        Button rightArrow = new Button(">");
        rightArrow.setPrefWidth(30);
        rightArrow.setPrefHeight(50);
        rightArrow.setFont(Font.font("Arial", FontWeight.BOLD, 20));
        rightArrow.setStyle("-fx-background-color: rgba(100, 100, 100, 0.7); " +
                "-fx-text-fill: white; " +
                "-fx-background-radius: 5; " +
                "-fx-border-color: rgba(200, 200, 200, 0.5); " +
                "-fx-border-width: 2; " +
                "-fx-border-radius: 5;");

        rightArrow.setOnMouseEntered(e -> {
            rightArrow.setStyle("-fx-background-color: rgba(120, 120, 120, 0.8); " +
                    "-fx-text-fill: white; " +
                    "-fx-background-radius: 5; " +
                    "-fx-border-color: rgba(200, 200, 200, 0.7); " +
                    "-fx-border-width: 2; " +
                    "-fx-border-radius: 5;");
        });

        rightArrow.setOnMouseExited(e -> {
            rightArrow.setStyle("-fx-background-color: rgba(100, 100, 100, 0.7); " +
                    "-fx-text-fill: white; " +
                    "-fx-background-radius: 5; " +
                    "-fx-border-color: rgba(200, 200, 200, 0.5); " +
                    "-fx-border-width: 2; " +
                    "-fx-border-radius: 5;");
        });

        // Update bars based on current difficulty
        Runnable updateBars = () -> {
            for (int i = 0; i < 10; i++) {
                if (difficulty > 0 && i < difficulty) {
                    double ratio = (i + 1) / 10.0;
                    Color barColor = Color.rgb(
                            (int)(255 * ratio),           // Red increases
                            (int)(255 * (1 - ratio)),     // Green decreases
                            0
                    );
                    bars[i].setFill(barColor);
                    bars[i].setStroke(Color.WHITE);
                    bars[i].setStrokeWidth(1);
                } else {
                    // Empty bar
                    bars[i].setFill(Color.rgb(50, 50, 50, 0.5));
                    bars[i].setStroke(Color.rgb(100, 100, 100, 0.5));
                    bars[i].setStrokeWidth(1);
                }
            }
        };

        updateBars.run();

        // Left arrow action(decrease)
        leftArrow.setOnAction(e -> {
            if (difficulty > 0) {
                difficulty--;
                if (difficultyLabel.getText().contains("Starting Difficulty")) {
                    difficultyLabel.setText("Starting Difficulty (0-10): " + difficulty);
                } else {
                    difficultyLabel.setText("Difficulty (0-10): " + difficulty);
                }
                updateBars.run();
            }
        });

        // Right arrow action(increase)
        rightArrow.setOnAction(e -> {
            if (difficulty < 10) {
                difficulty++;
                if (difficultyLabel.getText().contains("Starting Difficulty")) {
                    difficultyLabel.setText("Starting Difficulty (0-10): " + difficulty);
                } else {
                    difficultyLabel.setText("Difficulty (0-10): " + difficulty);
                }
                updateBars.run();
            }
        });

        container.getChildren().addAll(leftArrow, barsContainer, rightArrow);
        return container;
    }
}