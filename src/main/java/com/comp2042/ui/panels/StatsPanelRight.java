package com.comp2042.ui.panels;

import javafx.geometry.Pos;
import javafx.scene.Group;
import javafx.scene.Scene;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.VBox;

public class StatsPanelRight {

    private static final int PANEL_WIDTH = 300;

    private VBox container;
    private HBox targetRow;
    private HBox scoreRow;
    private Label targetValueLabel;
    private Label scoreValueLabel;

    public StatsPanelRight() {
        container = new VBox(10);
        container.setAlignment(Pos.TOP_LEFT);
        container.setSpacing(10);

        // Target row
        targetRow = new HBox(10);
        targetRow.getStyleClass().add("stat-row");
        targetRow.setAlignment(Pos.CENTER_LEFT);
        targetRow.setPrefWidth(PANEL_WIDTH);
        targetRow.setMinWidth(PANEL_WIDTH);
        targetRow.setMaxWidth(Double.MAX_VALUE);

        Label targetLabelText = new Label("TARGET:");
        targetLabelText.getStyleClass().add("stat-label-large");

        targetValueLabel = new Label("0/0");
        targetValueLabel.getStyleClass().add("stat-value-large");

        targetRow.getChildren().addAll(targetLabelText, targetValueLabel);

        // Score row
        scoreRow = new HBox(10);
        scoreRow.getStyleClass().add("stat-row");
        scoreRow.setAlignment(Pos.CENTER_LEFT);
        scoreRow.setPrefWidth(PANEL_WIDTH);
        scoreRow.setMinWidth(PANEL_WIDTH);
        scoreRow.setMaxWidth(Double.MAX_VALUE);

        Label labelText = new Label("SCORE:");
        labelText.getStyleClass().add("stat-label-large");

        scoreValueLabel = new Label("0");
        scoreValueLabel.getStyleClass().add("stat-value-large");

        scoreRow.getChildren().addAll(labelText, scoreValueLabel);

        container.getChildren().addAll(targetRow, scoreRow);
    }

    public void addToScene(Scene scene) {
        if (scene != null && container != null) {
            Pane rootPane = (Pane) scene.getRoot();
            rootPane.getChildren().add(container);
        }
    }

    public void addToGameplayLayer(Group gameplayLayer) {
        if (gameplayLayer != null && container != null) {
            gameplayLayer.getChildren().add(container);
        }
    }

    public void position(double boardX, double boardY, double boardWidth, double boardHeight) {
        if (container == null) return;

        // Position under the next panel on the right side
        double nextPanelX = boardX + boardWidth + 30;
        double nextPanelHeight = boardHeight / 2;
        double containerX = nextPanelX;
        double containerY = boardY + nextPanelHeight + 200;

        container.setLayoutX(containerX);
        container.setLayoutY(containerY);
    }

    public void updateScore(int score) {
        if (scoreValueLabel != null) {
            scoreValueLabel.setText(String.valueOf(score));
        }
    }

    public void updateTarget(int current, int target) {
        if (targetValueLabel != null) {
            targetValueLabel.setText(current + "/" + target);
        }
    }

    public HBox getScoreRow() {
        return scoreRow;
    }
}