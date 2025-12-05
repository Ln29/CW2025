package com.comp2042.ui.panels;

import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;

/**
 * Left stats panel displaying game mode, time, level, and high score.
 */
public class StatsPanel extends VBox {

    private static final int PANEL_WIDTH = 300;
    private static final int ROW_SPACING = 10;

    private Label modeValueLabel;
    private Label timeValueLabel;
    private Label levelValueLabel;
    private Label highScoreValueLabel;

    public StatsPanel() {
        setAlignment(Pos.TOP_CENTER);
        setSpacing(ROW_SPACING);
        setPadding(new Insets(15, 10, 15, 10));
        setPrefWidth(PANEL_WIDTH);
        setMinWidth(PANEL_WIDTH);
        setMaxWidth(PANEL_WIDTH);

        createStatRow("MODE", "Endless");
        createStatRow("TIME", "00:00");
        createStatRow("LEVEL", "1");
        createStatRow("HIGH SCORE", "0");
    }

    private void createStatRow(String label, String initialValue) {
        HBox row = new HBox(10);
        row.getStyleClass().add("stat-row");
        row.setAlignment(Pos.CENTER_RIGHT);
        row.setMaxWidth(Double.MAX_VALUE);

        Label labelText = new Label(label + ":");
        labelText.getStyleClass().add("stat-label");

        Label valueText = new Label(initialValue);
        valueText.getStyleClass().add("stat-value");

        row.getChildren().addAll(labelText, valueText);
        getChildren().add(row);

        switch (label) {
            case "MODE":
                modeValueLabel = valueText;
                break;
            case "TIME":
                timeValueLabel = valueText;
                break;
            case "LEVEL":
                levelValueLabel = valueText;
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

    public void updateHighScore(int highScore) {
        if (highScoreValueLabel != null) {
            highScoreValueLabel.setText(String.valueOf(highScore));
        }
    }

    public void updateMode(String mode) {
        if (modeValueLabel != null) {
            modeValueLabel.setText(mode);
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