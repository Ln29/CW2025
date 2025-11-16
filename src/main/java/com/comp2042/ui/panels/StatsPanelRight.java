package com.comp2042.ui.panels;

import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.paint.Color;
import javafx.scene.text.Font;
import javafx.scene.text.FontWeight;

public class StatsPanelRight {

    private static final int PANEL_WIDTH = 300;
    private static final int ROW_HEIGHT = 50;

    private HBox scoreRow;
    private Label scoreValueLabel;

    public StatsPanelRight() {
        scoreRow = new HBox(10);
        scoreRow.setAlignment(Pos.CENTER_LEFT);
        scoreRow.setPadding(new Insets(8, 12, 8, 12));
        scoreRow.setPrefHeight(ROW_HEIGHT);
        scoreRow.setMinHeight(ROW_HEIGHT);
        scoreRow.setMaxHeight(ROW_HEIGHT);
        scoreRow.setPrefWidth(PANEL_WIDTH);
        scoreRow.setMinWidth(PANEL_WIDTH);
        scoreRow.setMaxWidth(Double.MAX_VALUE);
        scoreRow.setStyle("-fx-background-color: rgba(0, 0, 0, 0.5); -fx-background-radius: 5;");

        // Create label text "SCORE:"
        Label labelText = new Label("SCORE:");
        labelText.setTextFill(Color.WHITE);
        labelText.setFont(Font.font("Arial", 25));

        // Create value label
        scoreValueLabel = new Label("0");
        scoreValueLabel.setTextFill(Color.WHITE);
        scoreValueLabel.setFont(Font.font("Arial", FontWeight.BOLD, 25));

        scoreRow.getChildren().addAll(labelText, scoreValueLabel);
    }

    public void addToScene(Scene scene) {
        if (scene != null && scoreRow != null) {
            Pane rootPane = (Pane) scene.getRoot();
            rootPane.getChildren().add(scoreRow);
        }
    }

    public void position(double boardX, double boardY, double boardWidth, double boardHeight) {
        if (scoreRow == null) return;

        // Position under the next panel on the right side
        double nextPanelX = boardX + boardWidth + 30;
        double nextPanelHeight = boardHeight / 2;
        double scoreX = nextPanelX;
        double scoreY = boardY + nextPanelHeight + 200; // 200px gap below next panel

        scoreRow.setLayoutX(scoreX);
        scoreRow.setLayoutY(scoreY);
    }

    public void updateScore(int score) {
        if (scoreValueLabel != null) {
            scoreValueLabel.setText(String.valueOf(score));
        }
    }

    public HBox getScoreRow() {
        return scoreRow;
    }
}

