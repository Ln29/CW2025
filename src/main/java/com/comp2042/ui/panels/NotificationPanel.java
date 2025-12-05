package com.comp2042.ui.panels;

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
import javafx.util.Duration;

/**
 * Animated notification panel for displaying combo, score bonus, and level up messages.
 */
public class NotificationPanel extends BorderPane {

    private Label label;
    private boolean isCombo;
    private boolean isLevelUp;

    public NotificationPanel(String text) {
        this(text, false, false);
    }

    public NotificationPanel(String text, boolean isCombo) {
        this(text, isCombo, false);
    }

    public NotificationPanel(String text, boolean isCombo, boolean isLevelUp) {
        this.isCombo = isCombo;
        this.isLevelUp = isLevelUp;
        setMinHeight(200);
        setMinWidth(220);
        label = new Label(text);
        label.getStyleClass().add("bonusStyle");
        final Effect glow = new Glow(0.6);
        label.setEffect(glow);

        if (isLevelUp) {
            label.getStyleClass().add("notification-levelup");
        } else if (isCombo) {
            label.getStyleClass().add("notification-combo");
        } else {
            label.getStyleClass().add("notification-score");
        }

        setCenter(label);
    }

    public void updateText(String newText) {
        if (label != null) {
            label.setText(newText);
        }
    }

    public void showScore(ObservableList<Node> list) {
        if (!isCombo && !isLevelUp) {
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

    public void showLevelUp(ObservableList<Node> list) {
        this.setOpacity(0);
        FadeTransition fadeIn = new FadeTransition(Duration.millis(300), this);
        fadeIn.setFromValue(0);
        fadeIn.setToValue(1);

        FadeTransition fadeOut = new FadeTransition(Duration.millis(500), this);
        fadeOut.setFromValue(1);
        fadeOut.setToValue(0);
        fadeOut.setDelay(Duration.millis(1500));

        fadeIn.setOnFinished(new EventHandler<ActionEvent>() {
            @Override
            public void handle(ActionEvent event) {
                fadeOut.play();
            }
        });

        fadeOut.setOnFinished(new EventHandler<ActionEvent>() {
            @Override
            public void handle(ActionEvent event) {
                list.remove(NotificationPanel.this);
            }
        });

        fadeIn.play();
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

