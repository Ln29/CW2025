package com.comp2042.ui.menu;

import com.comp2042.config.GameModeConfig;
import com.comp2042.config.KeyBindingsConfig;
import com.comp2042.core.GameMode;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.control.*;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
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

    // Keyboard navigation
    private enum NavigationState {
        MODE,      // Selecting game mode (default)
        OPTIONS,   // Adjusting mode-specific options
        ACTION     // Selecting action button (START/BACK)
    }

    private enum MarathonOptionsState {
        TARGET,     // Navigating target lines
        DIFFICULTY  // Navigating difficulty
    }

    private NavigationState navigationState = NavigationState.MODE;
    private MarathonOptionsState marathonOptionsState = MarathonOptionsState.TARGET; // For Marathon mode only
    private int selectedModeIndex = 0; // 0=ENDLESS, 1=MARATHON, 2=SURVIVAL
    private int selectedActionIndex = 0; // 0=START, 1=BACK
    private Button[] modeButtonsArray;
    private Button[] actionButtonsArray;

    // References for options navigation
    private Button leftArrowButton; // Reference to left arrow button for difficulty
    private Button rightArrowButton; // Reference to right arrow button for difficulty
    private HBox currentTargetButtons; // Reference to target buttons container (Marathon mode)
    private HBox currentSurvivalDifficultyButtons; // Reference to survival difficulty buttons container

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

        Label titleLabel = new Label("SELECT GAME MODE");
        titleLabel.setTextFill(Color.YELLOW);
        titleLabel.setFont(Font.font("Arial", FontWeight.BOLD, 36));
        titleLabel.setPadding(new Insets(0, 0, 20, 0));

        HBox modeButtons = new HBox(15);
        modeButtons.setAlignment(Pos.CENTER);

        Button endlessButton = createModeButton("ENDLESS", GameMode.ENDLESS);
        Button marathonButton = createModeButton("MARATHON", GameMode.MARATHON);
        Button survivalButton = createModeButton("SURVIVAL", GameMode.SURVIVAL);

        modeButtonsArray = new Button[]{endlessButton, marathonButton, survivalButton};
        modeButtons.getChildren().addAll(endlessButton, marathonButton, survivalButton);

        modeOptionsContainer = new VBox(15);
        modeOptionsContainer.setAlignment(Pos.CENTER);
        modeOptionsContainer.setPadding(new Insets(20, 0, 20, 0));

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

        actionButtonsArray = new Button[]{startButton, backButton};
        actionButtons.getChildren().addAll(startButton, backButton);

        getChildren().addAll(titleLabel, modeButtons, modeOptionsContainer, actionButtons);

        setOnKeyPressed(this::handleKeyPress);
        setFocusTraversable(true);

        updateModeOptions();
        updateModeButtonStyles(modeButtonsArray[selectedModeIndex]);
        updateActionButtonStyles();
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
            for (int i = 0; i < modeButtonsArray.length; i++) {
                if (modeButtonsArray[i] == button) {
                    selectedModeIndex = i;
                    break;
                }
            }
            navigationState = NavigationState.MODE;
            marathonOptionsState = MarathonOptionsState.TARGET;
            updateModeOptions();
            updateModeButtonStyles(button);
            updateActionButtonStyles();
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
        if (modeButtonsArray == null) return;
        for (int i = 0; i < modeButtonsArray.length; i++) {
            Button btn = modeButtonsArray[i];
            if ((btn == selectedButton || i == selectedModeIndex) && navigationState == NavigationState.MODE) {
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

    private void updateActionButtonStyles() {
        if (actionButtonsArray == null) return;
        for (int i = 0; i < actionButtonsArray.length; i++) {
            Button btn = actionButtonsArray[i];
            if (i == selectedActionIndex && navigationState == NavigationState.ACTION) {
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

    private void updateOptionsHighlight() {
        if (navigationState == NavigationState.OPTIONS) {
            if (leftArrowButton != null && rightArrowButton != null && selectedMode == GameMode.ENDLESS) {
                leftArrowButton.setStyle("-fx-background-color: rgba(255, 215, 0, 0.9); " +
                        "-fx-text-fill: black; " +
                        "-fx-background-radius: 5; " +
                        "-fx-border-color: rgba(255, 255, 255, 0.9); " +
                        "-fx-border-width: 3; " +
                        "-fx-border-radius: 5;");
                rightArrowButton.setStyle("-fx-background-color: rgba(255, 215, 0, 0.9); " +
                        "-fx-text-fill: black; " +
                        "-fx-background-radius: 5; " +
                        "-fx-border-color: rgba(255, 255, 255, 0.9); " +
                        "-fx-border-width: 3; " +
                        "-fx-border-radius: 5;");
            }

            if (selectedMode == GameMode.MARATHON) {
                if (marathonOptionsState == MarathonOptionsState.TARGET) {
                    if (currentTargetButtons != null) {
                        updateTargetButtonHighlight();
                    }
                    if (leftArrowButton != null && rightArrowButton != null) {
                        leftArrowButton.setStyle("-fx-background-color: rgba(100, 100, 100, 0.7); " +
                                "-fx-text-fill: white; " +
                                "-fx-background-radius: 5; " +
                                "-fx-border-color: rgba(200, 200, 200, 0.5); " +
                                "-fx-border-width: 2; " +
                                "-fx-border-radius: 5;");
                        rightArrowButton.setStyle("-fx-background-color: rgba(100, 100, 100, 0.7); " +
                                "-fx-text-fill: white; " +
                                "-fx-background-radius: 5; " +
                                "-fx-border-color: rgba(200, 200, 200, 0.5); " +
                                "-fx-border-width: 2; " +
                                "-fx-border-radius: 5;");
                    }
                } else if (marathonOptionsState == MarathonOptionsState.DIFFICULTY) {
                    if (leftArrowButton != null && rightArrowButton != null) {
                        leftArrowButton.setStyle("-fx-background-color: rgba(255, 215, 0, 0.9); " +
                                "-fx-text-fill: black; " +
                                "-fx-background-radius: 5; " +
                                "-fx-border-color: rgba(255, 255, 255, 0.9); " +
                                "-fx-border-width: 3; " +
                                "-fx-border-radius: 5;");
                        rightArrowButton.setStyle("-fx-background-color: rgba(255, 215, 0, 0.9); " +
                                "-fx-text-fill: black; " +
                                "-fx-background-radius: 5; " +
                                "-fx-border-color: rgba(255, 255, 255, 0.9); " +
                                "-fx-border-width: 3; " +
                                "-fx-border-radius: 5;");
                    }
                    if (currentTargetButtons != null) {
                        updateTargetButtonHighlight();
                    }
                }
            }

            if (currentSurvivalDifficultyButtons != null && selectedMode == GameMode.SURVIVAL) {
                updateSurvivalDifficultyButtonHighlight();
            }
        } else {
            if (leftArrowButton != null && rightArrowButton != null) {
                leftArrowButton.setStyle("-fx-background-color: rgba(100, 100, 100, 0.7); " +
                        "-fx-text-fill: white; " +
                        "-fx-background-radius: 5; " +
                        "-fx-border-color: rgba(200, 200, 200, 0.5); " +
                        "-fx-border-width: 2; " +
                        "-fx-border-radius: 5;");
                rightArrowButton.setStyle("-fx-background-color: rgba(100, 100, 100, 0.7); " +
                        "-fx-text-fill: white; " +
                        "-fx-background-radius: 5; " +
                        "-fx-border-color: rgba(200, 200, 200, 0.5); " +
                        "-fx-border-width: 2; " +
                        "-fx-border-radius: 5;");
            }
        }
    }

    private void updateTargetButtonHighlight() {
        if (currentTargetButtons == null) return;
        for (var child : currentTargetButtons.getChildren()) {
            if (child instanceof Button) {
                Button btn = (Button) child;
                int targetValue = Integer.parseInt(btn.getText());
                if (targetValue == marathonTargetLines && navigationState == NavigationState.OPTIONS) {
                    btn.setStyle("-fx-background-color: rgba(255, 215, 0, 0.9); -fx-text-fill: black; " +
                            "-fx-border-color: rgba(255, 255, 255, 0.9); " +
                            "-fx-border-width: 3;");
                } else if (targetValue == marathonTargetLines) {
                    btn.setStyle("-fx-background-color: rgba(255, 215, 0, 0.9); -fx-text-fill: black;");
                } else {
                    btn.setStyle("-fx-background-color: rgba(100, 100, 100, 0.7); -fx-text-fill: white;");
                }
            }
        }
    }

    private void updateSurvivalDifficultyButtonHighlight() {
        if (currentSurvivalDifficultyButtons == null) return;
        for (var child : currentSurvivalDifficultyButtons.getChildren()) {
            if (child instanceof Button) {
                Button btn = (Button) child;
                GameModeConfig.SurvivalDifficulty btnDiff = GameModeConfig.SurvivalDifficulty.valueOf(btn.getText());
                if (btnDiff == survivalDifficulty && navigationState == NavigationState.OPTIONS) {
                    btn.setStyle("-fx-background-color: rgba(255, 215, 0, 0.9); -fx-text-fill: black; " +
                            "-fx-border-color: rgba(255, 255, 255, 0.9); " +
                            "-fx-border-width: 3;");
                } else if (btnDiff == survivalDifficulty) {
                    btn.setStyle("-fx-background-color: rgba(255, 215, 0, 0.9); -fx-text-fill: black;");
                } else {
                    btn.setStyle("-fx-background-color: rgba(100, 100, 100, 0.7); -fx-text-fill: white;");
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
        updateOptionsHighlight();
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

        for (int target : new int[]{5, 100, 200, 500}) {
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

        currentTargetButtons = targetButtons;
        modeOptionsContainer.getChildren().addAll(description, targetLabel, targetButtons, difficultyLabel, difficultySelector);
        updateOptionsHighlight();
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
        Label description = new Label();
        description.setTextFill(Color.WHITE);
        description.setFont(Font.font("Arial", 14));
        description.setAlignment(Pos.CENTER);

        Label details = new Label();
        details.setTextFill(Color.LIGHTGRAY);
        details.setFont(Font.font("Arial", 12));

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

        currentSurvivalDifficultyButtons = difficultyButtons;
        modeOptionsContainer.getChildren().addAll(description, difficultyLabel, difficultyButtons, details);
        updateOptionsHighlight();
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

    public void requestFocusForNavigation() {
        requestFocus();
        navigationState = NavigationState.MODE;
        marathonOptionsState = MarathonOptionsState.TARGET;
        if (modeButtonsArray != null && selectedModeIndex >= 0 && selectedModeIndex < modeButtonsArray.length) {
            updateModeButtonStyles(modeButtonsArray[selectedModeIndex]);
            updateActionButtonStyles();
            updateOptionsHighlight();
        }
    }

    private void handleKeyPress(KeyEvent event) {
        KeyCode code = event.getCode();
        KeyBindingsConfig config = KeyBindingsConfig.getInstance();

        // Check if the pressed key matches any bound action
        KeyBindingsConfig.Action action = config.getAction(code);
        boolean isLeft = (code == KeyCode.LEFT) || (action == KeyBindingsConfig.Action.MOVE_LEFT);
        boolean isRight = (code == KeyCode.RIGHT) || (action == KeyBindingsConfig.Action.MOVE_RIGHT);
        boolean isUp = (code == KeyCode.UP) || (action == KeyBindingsConfig.Action.ROTATE);
        boolean isDown = (code == KeyCode.DOWN) || (action == KeyBindingsConfig.Action.SOFT_DROP);
        boolean isSelect = (code == KeyCode.ENTER || code == KeyCode.SPACE) || (action == KeyBindingsConfig.Action.HARD_DROP);
        boolean isBack = (code == KeyCode.ESCAPE) || (action == KeyBindingsConfig.Action.PAUSE);

        if (isBack) {
            if (navigationState == NavigationState.ACTION) {
                navigationState = NavigationState.OPTIONS;
                if (selectedMode == GameMode.MARATHON) {
                    marathonOptionsState = MarathonOptionsState.DIFFICULTY;
                }
                updateActionButtonStyles();
                updateOptionsHighlight();
            } else if (navigationState == NavigationState.OPTIONS) {
                if (selectedMode == GameMode.MARATHON && marathonOptionsState == MarathonOptionsState.DIFFICULTY) {
                    marathonOptionsState = MarathonOptionsState.TARGET;
                    updateOptionsHighlight();
                } else {
                    navigationState = NavigationState.MODE;
                    marathonOptionsState = MarathonOptionsState.TARGET;
                    updateModeButtonStyles(modeButtonsArray[selectedModeIndex]);
                    updateOptionsHighlight();
                }
            } else {
                if (onBack != null) {
                    onBack.run();
                }
            }
            event.consume();
            return;
        }

        if (isUp) {
            if (navigationState == NavigationState.ACTION) {
                navigationState = NavigationState.OPTIONS;
                if (selectedMode == GameMode.MARATHON) {
                    marathonOptionsState = MarathonOptionsState.DIFFICULTY;
                }
                updateActionButtonStyles();
                updateOptionsHighlight();
            } else if (navigationState == NavigationState.OPTIONS) {
                if (selectedMode == GameMode.MARATHON && marathonOptionsState == MarathonOptionsState.DIFFICULTY) {
                    marathonOptionsState = MarathonOptionsState.TARGET;
                    updateOptionsHighlight();
                } else {
                    navigationState = NavigationState.MODE;
                    marathonOptionsState = MarathonOptionsState.TARGET;
                    updateModeButtonStyles(modeButtonsArray[selectedModeIndex]);
                    updateOptionsHighlight();
                }
            }
            event.consume();
            return;
        } else if (isDown) {
            if (navigationState == NavigationState.MODE) {
                navigationState = NavigationState.OPTIONS;
                marathonOptionsState = MarathonOptionsState.TARGET;
                updateModeButtonStyles(modeButtonsArray[selectedModeIndex]);
                updateOptionsHighlight();
            } else if (navigationState == NavigationState.OPTIONS) {
                if (selectedMode == GameMode.MARATHON && marathonOptionsState == MarathonOptionsState.TARGET) {
                    marathonOptionsState = MarathonOptionsState.DIFFICULTY;
                    updateOptionsHighlight();
                } else {
                    navigationState = NavigationState.ACTION;
                    updateActionButtonStyles();
                    updateOptionsHighlight();
                }
            }
            event.consume();
            return;
        }

        switch (navigationState) {
            case MODE:
                handleModeNavigation(isLeft, isRight, isSelect, event);
                break;
            case OPTIONS:
                handleOptionsNavigation(isLeft, isRight, isSelect, event);
                break;
            case ACTION:
                handleActionNavigation(isLeft, isRight, isSelect, event);
                break;
        }
    }

    private void handleModeNavigation(boolean isLeft, boolean isRight, boolean isSelect, KeyEvent event) {
        if (isLeft && selectedModeIndex > 0) {
            selectedModeIndex--;
            GameMode[] modes = {GameMode.ENDLESS, GameMode.MARATHON, GameMode.SURVIVAL};
            selectedMode = modes[selectedModeIndex];
            updateModeOptions();
            updateModeButtonStyles(modeButtonsArray[selectedModeIndex]);
            event.consume();
            return;
        } else if (isRight && selectedModeIndex < modeButtonsArray.length - 1) {
            selectedModeIndex++;
            GameMode[] modes = {GameMode.ENDLESS, GameMode.MARATHON, GameMode.SURVIVAL};
            selectedMode = modes[selectedModeIndex];
            updateModeOptions();
            updateModeButtonStyles(modeButtonsArray[selectedModeIndex]);
            event.consume();
            return;
        }

        if (isSelect) {
            navigationState = NavigationState.OPTIONS;
            marathonOptionsState = MarathonOptionsState.TARGET;
            updateModeButtonStyles(modeButtonsArray[selectedModeIndex]);
            updateOptionsHighlight();
            event.consume();
        }
    }

    private void handleOptionsNavigation(boolean isLeft, boolean isRight, boolean isSelect, KeyEvent event) {
        if (selectedMode == GameMode.MARATHON) {
            if (marathonOptionsState == MarathonOptionsState.TARGET) {
                int[] targets = {5, 100, 200, 500};
                int currentTargetIndex = -1;
                for (int i = 0; i < targets.length; i++) {
                    if (targets[i] == marathonTargetLines) {
                        currentTargetIndex = i;
                        break;
                    }
                }
                if (currentTargetIndex >= 0) {
                    if (isLeft && currentTargetIndex > 0) {
                        marathonTargetLines = targets[currentTargetIndex - 1];
                        updateMarathonTargetLabel();
                        event.consume();
                        return;
                    } else if (isRight && currentTargetIndex < targets.length - 1) {
                        marathonTargetLines = targets[currentTargetIndex + 1];
                        updateMarathonTargetLabel();
                        event.consume();
                        return;
                    }
                }
                if (isSelect) {
                    marathonOptionsState = MarathonOptionsState.DIFFICULTY;
                    updateOptionsHighlight();
                    event.consume();
                    return;
                }
            } else if (marathonOptionsState == MarathonOptionsState.DIFFICULTY) {
                if (isLeft && difficulty > 0) {
                    difficulty--;
                    updateDifficultyLabel();
                    event.consume();
                    return;
                } else if (isRight && difficulty < 10) {
                    difficulty++;
                    updateDifficultyLabel();
                    event.consume();
                    return;
                }
                // Enter moves from DIFFICULTY to ACTION
                if (isSelect) {
                    navigationState = NavigationState.ACTION;
                    selectedActionIndex = 0; // Select START button
                    updateOptionsHighlight();
                    updateActionButtonStyles();
                    event.consume();
                    return;
                }
            }
        } else if (selectedMode == GameMode.ENDLESS) {
            // Adjust difficulty with Left/Right for Endless mode
            if (isLeft && difficulty > 0) {
                difficulty--;
                updateDifficultyLabel();
                event.consume();
                return;
            } else if (isRight && difficulty < 10) {
                difficulty++;
                updateDifficultyLabel();
                event.consume();
                return;
            }
        } else if (selectedMode == GameMode.SURVIVAL) {
            GameModeConfig.SurvivalDifficulty[] diffs = GameModeConfig.SurvivalDifficulty.values();
            int currentIndex = -1;
            for (int i = 0; i < diffs.length; i++) {
                if (diffs[i] == survivalDifficulty) {
                    currentIndex = i;
                    break;
                }
            }
            if (currentIndex >= 0) {
                if (isLeft && currentIndex > 0) {
                    survivalDifficulty = diffs[currentIndex - 1];
                    updateSurvivalOptions();
                    event.consume();
                    return;
                } else if (isRight && currentIndex < diffs.length - 1) {
                    survivalDifficulty = diffs[currentIndex + 1];
                    updateSurvivalOptions();
                    event.consume();
                    return;
                }
            }
        }

        if (isSelect && selectedMode != GameMode.MARATHON) {
            navigationState = NavigationState.ACTION;
            selectedActionIndex = 0;
            updateOptionsHighlight();
            updateActionButtonStyles();
            event.consume();
        }
    }

    private void handleActionNavigation(boolean isLeft, boolean isRight, boolean isSelect, KeyEvent event) {
        if (isLeft && selectedActionIndex > 0) {
            selectedActionIndex--;
            updateActionButtonStyles();
            event.consume();
            return;
        } else if (isRight && selectedActionIndex < actionButtonsArray.length - 1) {
            selectedActionIndex++;
            updateActionButtonStyles();
            event.consume();
            return;
        }

        if (isSelect) {
            actionButtonsArray[selectedActionIndex].fire();
            event.consume();
        }
    }

    private void updateDifficultyLabel() {
        updateModeOptions();
        updateOptionsHighlight();
    }

    private void updateSurvivalOptions() {
        updateModeOptions();
        updateOptionsHighlight();
    }

    private void updateMarathonTargetLabel() {
        if (currentTargetButtons == null) return;

        for (var child : modeOptionsContainer.getChildren()) {
            if (child instanceof Label) {
                Label label = (Label) child;
                if (label.getText().contains("Target Lines")) {
                    label.setText("Target Lines: " + marathonTargetLines);
                    break;
                }
            }
        }

        // Update button styles
        updateTargetButtonHighlight();
    }

    /**
     * Creates a volume bar style difficulty selector with arrow buttons and vertical bars
     * Layout: < [10 vertical bars] >
     * Difficulty 0 = all grey, Difficulty 1-10 = 1-10 bars filled
     */
    private HBox createVolumeBarDifficultySelector(Label difficultyLabel) {
        HBox container = new HBox(10);
        container.setAlignment(Pos.CENTER);

        Button leftArrow = new Button("<");
        leftArrowButton = leftArrow;
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

        Rectangle[] bars = new Rectangle[10];
        for (int i = 0; i < 10; i++) {
            Rectangle bar = new Rectangle(12, 50);
            bar.setArcWidth(3);
            bar.setArcHeight(3);
            bars[i] = bar;
            barsContainer.getChildren().add(bar);
        }

        Button rightArrow = new Button(">");
        rightArrowButton = rightArrow;
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

        Runnable updateBars = () -> {
            for (int i = 0; i < 10; i++) {
                if (difficulty > 0 && i < difficulty) {
                    double ratio = (i + 1) / 10.0;
                    Color barColor = Color.rgb(
                            (int)(255 * ratio),
                            (int)(255 * (1 - ratio)),
                            0
                    );
                    bars[i].setFill(barColor);
                    bars[i].setStroke(Color.WHITE);
                    bars[i].setStrokeWidth(1);
                } else {
                    bars[i].setFill(Color.rgb(50, 50, 50, 0.5));
                    bars[i].setStroke(Color.rgb(100, 100, 100, 0.5));
                    bars[i].setStrokeWidth(1);
                }
            }
        };

        updateBars.run();

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
