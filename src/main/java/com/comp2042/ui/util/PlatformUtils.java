package com.comp2042.ui.util;

import javafx.application.Platform;

/**
 * Utility class for JavaFX platform operations.
 * Provides thread-safe access to the JavaFX Application Thread.
 */
public final class PlatformUtils {

    private PlatformUtils() {}

    /**
     * Executes the given Runnable on the JavaFX Application Thread.
     * This is a convenience method that wraps Platform.runLater().
     * 
     * @param action The action to execute on the JavaFX Application Thread
     */
    public static void run(Runnable action) {
        Platform.runLater(action);
    }
}

