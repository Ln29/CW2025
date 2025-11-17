package com.comp2042.ui.panels;

import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Pane;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;
import javafx.scene.text.Font;
import javafx.scene.text.FontWeight;

public class StatsPanelRight {

    private static final int PANEL_WIDTH = 300;
    private static final int ROW_HEIGHT = 50;

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
        targetRow.setAlignment(Pos.CENTER_LEFT);
        targetRow.setPadding(new Insets(8, 12, 8, 12));
        targetRow.setPrefHeight(ROW_HEIGHT);
        targetRow.setMinHeight(ROW_HEIGHT);
        targetRow.setMaxHeight(ROW_HEIGHT);
        targetRow.setPrefWidth(PANEL_WIDTH);
        targetRow.setMinWidth(PANEL_WIDTH);
        targetRow.setMaxWidth(Double.MAX_VALUE);
        targetRow.setStyle("-fx-background-color: rgba(0, 0, 0, 0.7); -fx-background-radius: 5;");

        Label targetLabelText = new Label("TARGET:");
        targetLabelText.setTextFill(Color.WHITE);
        targetLabelText.setFont(Font.font("Arial", 25));

        targetValueLabel = new Label("0/0");
        targetValueLabel.setTextFill(Color.WHITE);
        targetValueLabel.setFont(Font.font("Arial", FontWeight.BOLD, 25));

        targetRow.getChildren().addAll(targetLabelText, targetValueLabel);

        // Score row
        scoreRow = new HBox(10);
        scoreRow.setAlignment(Pos.CENTER_LEFT);
        scoreRow.setPadding(new Insets(8, 12, 8, 12));
        scoreRow.setPrefHeight(ROW_HEIGHT);
        scoreRow.setMinHeight(ROW_HEIGHT);
        scoreRow.setMaxHeight(ROW_HEIGHT);
        scoreRow.setPrefWidth(PANEL_WIDTH);
        scoreRow.setMinWidth(PANEL_WIDTH);
        scoreRow.setMaxWidth(Double.MAX_VALUE);
        scoreRow.setStyle("-fx-background-color: rgba(0, 0, 0, 0.7); -fx-background-radius: 5;");

        Label labelText = new Label("SCORE:");
        labelText.setTextFill(Color.WHITE);
        labelText.setFont(Font.font("Arial", 25));

        scoreValueLabel = new Label("0");
        scoreValueLabel.setTextFill(Color.WHITE);
        scoreValueLabel.setFont(Font.font("Arial", FontWeight.BOLD, 25));

        scoreRow.getChildren().addAll(labelText, scoreValueLabel);

        container.getChildren().addAll(targetRow, scoreRow);
    }

    public void addToScene(Scene scene) {
        if (scene != null && container != null) {
            Pane rootPane = (Pane) scene.getRoot();
            rootPane.getChildren().add(container);
        }
    }

    public void position(double boardX, double boardY, double boardWidth, double boardHeight) {
        if (container == null) return;

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