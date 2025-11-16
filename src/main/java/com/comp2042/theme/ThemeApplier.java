package com.comp2042.theme;

import com.comp2042.audio.AudioManager;
import com.comp2042.ui.menu.ThemeMenu;
import javafx.application.Platform;
import javafx.scene.Scene;
import javafx.scene.layout.Background;
import javafx.scene.layout.BackgroundImage;
import javafx.scene.layout.BackgroundPosition;
import javafx.scene.layout.BackgroundRepeat;
import javafx.scene.layout.BackgroundSize;

/**
 * Applies a theme consistently: updates ThemeConfig, background image, music, then triggers UI refresh.
 */
public class ThemeApplier {

    private final ThemeConfig themeConfig;
    private final AudioManager audioManager;

    public ThemeApplier(ThemeConfig themeConfig, AudioManager audioManager) {
        this.themeConfig = themeConfig;
        this.audioManager = audioManager;
    }

    public void apply(ThemeMenu.Theme theme, Scene scene, Runnable refreshDisplays, boolean playGameMusic) {
        if (theme == null) return;
        // Update theme state
        if (themeConfig != null) {
            themeConfig.setTheme(theme);
        }

        // Update background image on scene root
        if (scene != null && theme.getImagePath() != null) {
            try {
                javafx.scene.image.Image bgImage = new javafx.scene.image.Image(
                    getClass().getClassLoader().getResource("assets/images/" + theme.getImagePath()).toExternalForm()
                );
                BackgroundImage backgroundImage = new BackgroundImage(
                    bgImage,
                    BackgroundRepeat.NO_REPEAT,
                    BackgroundRepeat.NO_REPEAT,
                    BackgroundPosition.CENTER,
                    new BackgroundSize(BackgroundSize.AUTO, BackgroundSize.AUTO, false, false, true, true)
                );
                Platform.runLater(() -> {
                    javafx.scene.layout.Pane root = (javafx.scene.layout.Pane) scene.getRoot();
                    root.setBackground(new Background(backgroundImage));
                });
            } catch (Exception e) {
                System.err.println("Error applying theme background: " + e.getMessage());
            }
        }

        // Play theme music (game music channel)
        if (playGameMusic && audioManager != null && theme.getMusicFile() != null) {
            audioManager.playGameMusic(theme.getMusicFile());
        }

        // Refresh bricks/panels/colors
        if (refreshDisplays != null) {
            refreshDisplays.run();
        }
    }
}


