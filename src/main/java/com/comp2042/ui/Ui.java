package com.comp2042.ui;

import javafx.application.Platform;

public final class Ui {

    private Ui() {}

    public static void run(Runnable action) {
        Platform.runLater(action);
    }
}


