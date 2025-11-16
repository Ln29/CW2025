package com.comp2042.ui.panels;

import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;
import javafx.scene.text.Font;
import javafx.scene.text.FontWeight;

public class StatsPanel extends VBox {

    private static final int PANEL_WIDTH = 300;
    private static final int ROW_HEIGHT = 50;
    private static final int ROW_SPACING = 10;

    private Label timeValueLabel;
    private Label levelValueLabel;
    private Label linesValueLabel;
    private Label highScoreValueLabel;

    public StatsPanel() {
        setAlignment(Pos.TOP_CENTER);
        setSpacing(ROW_SPACING);
        setPadding(new Insets(15, 10, 15, 10));
        setPrefWidth(PANEL_WIDTH);
        setMinWidth(PANEL_WIDTH);
        setMaxWidth(PANEL_WIDTH);

        createStatRow("TIME", "00:00");
        createStatRow("LEVEL", "1");
        createStatRow("LINES", "0");
        createStatRow("HIGH SCORE", "0");
    }

    private void createStatRow(String label, String initialValue) {
        HBox row = new HBox(10);
        row.setAlignment(Pos.CENTER_RIGHT);
        row.setPadding(new Insets(8, 12, 8, 12));
        row.setPrefHeight(ROW_HEIGHT);
        row.setMinHeight(ROW_HEIGHT);
        row.setMaxHeight(ROW_HEIGHT);
        row.setMaxWidth(Double.MAX_VALUE);
        row.setStyle("-fx-background-color: rgba(0, 0, 0, 0.5); -fx-background-radius: 5;");

        Label labelText = new Label(label + ":");
        labelText.setTextFill(Color.WHITE);
        labelText.setFont(Font.font("Arial", 14));

        Label valueText = new Label(initialValue);
        valueText.setTextFill(Color.WHITE);
        valueText.setFont(Font.font("Arial", FontWeight.BOLD, 16));

        row.getChildren().addAll(labelText, valueText);
        getChildren().add(row);

        switch (label) {
            case "TIME":
                timeValueLabel = valueText;
                break;
            case "LEVEL":
                levelValueLabel = valueText;
                break;
            case "LINES":
                linesValueLabel = valueText;
                break;
            case "HIGH SCORE":
                highScoreValueLabel = valueText;
                break;
        }
    }

    public void updateTime(String time) {
        if (timeValueLabel != null) {
            timeValueLabel.setText(time);
        }
    }

    public void updateLevel(int level) {
        if (levelValueLabel != null) {
            levelValueLabel.setText(String.valueOf(level));
        }
    }

    public void updateLines(int lines) {
        if (linesValueLabel != null) {
            linesValueLabel.setText(String.valueOf(lines));
        }
    }

    public void updateHighScore(int highScore) {
        if (highScoreValueLabel != null) {
            highScoreValueLabel.setText(String.valueOf(highScore));
        }
    }

    public void position(double boardX, double boardY, double boardWidth, double boardHeight) {
        double holdPanelX = boardX - 310;
        double holdPanelHeight = boardHeight / 4;
        double statsPanelX = holdPanelX;
        double statsPanelY = boardY + holdPanelHeight + 50; // 50px gap below hold panel

        setLayoutX(statsPanelX);
        setLayoutY(statsPanelY);
    }
}

