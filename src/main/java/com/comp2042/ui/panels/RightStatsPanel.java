package com.comp2042.ui.panels;

import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.control.Label;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;

/**
 * Right stats panel displaying target lines and current score.
 */
public class RightStatsPanel extends VBox {

    private static final int PANEL_WIDTH = 300;
    private static final int ROW_SPACING = 10;

    private Label targetValueLabel;
    private Label scoreValueLabel;

    public RightStatsPanel() {
        setAlignment(Pos.TOP_LEFT);
        setSpacing(ROW_SPACING);
        setPadding(new Insets(15, 10, 15, 10));
        setPrefWidth(PANEL_WIDTH);
        setMinWidth(PANEL_WIDTH);
        setMaxWidth(PANEL_WIDTH);

        createStatRow("TARGET", "0/0", true);
        createStatRow("SCORE", "0", true);
    }

    private void createStatRow(String label, String initialValue, boolean useLargeStyle) {
        HBox row = new HBox(10);
        row.getStyleClass().add("stat-row");
        row.setAlignment(Pos.CENTER_LEFT);
        row.setMaxWidth(Double.MAX_VALUE);

        Label labelText = new Label(label + ":");
        labelText.getStyleClass().add(useLargeStyle ? "stat-label-large" : "stat-label");

        Label valueText = new Label(initialValue);
        valueText.getStyleClass().add(useLargeStyle ? "stat-value-large" : "stat-value");

        row.getChildren().addAll(labelText, valueText);
        getChildren().add(row);

        switch (label) {
            case "TARGET":
                targetValueLabel = valueText;
                break;
            case "SCORE":
                scoreValueLabel = valueText;
                break;
        }
    }

    public void position(double boardX, double boardY, double boardWidth, double boardHeight) {
        // Position under the next panel on the right side
        double nextPanelX = boardX + boardWidth + 30;
        double nextPanelHeight = boardHeight / 2;
        double containerX = nextPanelX;
        double containerY = boardY + nextPanelHeight + 200;

        setLayoutX(containerX);
        setLayoutY(containerY);
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
        // Return the score row for any styling needs
        if (getChildren().size() > 1) {
            return (HBox) getChildren().get(1);
        }
        return null;
    }
}
