package com.comp2042.ui.notification;

import javafx.animation.FadeTransition;
import javafx.animation.ParallelTransition;
import javafx.animation.TranslateTransition;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.Node;
import javafx.scene.control.Label;
import javafx.scene.effect.Effect;
import javafx.scene.effect.Glow;
import javafx.scene.layout.BorderPane;
import javafx.scene.paint.Color;
import javafx.util.Duration;

public class NotificationPanel extends BorderPane {

    private Label label;
    private boolean isCombo;

    public NotificationPanel(String text) {
        this(text, false);
    }

    public NotificationPanel(String text, boolean isCombo) {
        this.isCombo = isCombo;
        setMinHeight(200);
        setMinWidth(220);
        label = new Label(text);
        label.getStyleClass().add("bonusStyle");
        final Effect glow = new Glow(0.6);
        label.setEffect(glow);

        if (isCombo) {
            label.setTextFill(Color.RED);
            label.setFont(javafx.scene.text.Font.font("Arial", 10));
        } else {
            label.setTextFill(Color.WHITE);
            label.setFont(javafx.scene.text.Font.font("Arial", 10));
        }

        setCenter(label);
    }

    public void updateText(String newText) {
        if (label != null) {
            label.setText(newText);
        }
    }

    public void showScore(ObservableList<Node> list) {
        if (!isCombo) {
            FadeTransition ft = new FadeTransition(Duration.millis(2000), this);
            TranslateTransition tt = new TranslateTransition(Duration.millis(2500), this);
            tt.setToY(this.getLayoutY() - 40);
            ft.setFromValue(1);
            ft.setToValue(0);
            ParallelTransition transition = new ParallelTransition(tt, ft);
            transition.setOnFinished(new EventHandler<ActionEvent>() {
                @Override
                public void handle(ActionEvent event) {
                    list.remove(NotificationPanel.this);
                }
            });
            transition.play();
        }
    }

    public void fadeOutAndRemove(ObservableList<Node> list) {
        FadeTransition ft = new FadeTransition(Duration.millis(500), this);
        ft.setFromValue(1);
        ft.setToValue(0);
        ft.setOnFinished(new EventHandler<ActionEvent>() {
            @Override
            public void handle(ActionEvent event) {
                list.remove(NotificationPanel.this);
            }
        });
        ft.play();
    }
}