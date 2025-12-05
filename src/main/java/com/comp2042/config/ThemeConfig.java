package com.comp2042.config;

import com.comp2042.ui.menu.ThemeMenu;
import javafx.scene.paint.Color;
import javafx.scene.paint.Paint;

/**
 * Singleton configuration for game themes.
 * Manages current theme, brick colors, background images, and music.
 */
public class ThemeConfig {
    
    private static ThemeConfig instance;
    private ThemeMenu.Theme currentTheme = ThemeMenu.Theme.DEFAULT;
    private String[] currentBrickColors;
    private String currentBackgroundImage;
    private String currentMusicFile;
    
    private ThemeConfig() {
        updateTheme(ThemeMenu.Theme.DEFAULT);
    }
    
    /**
     * Gets the singleton ThemeConfig instance.
     * 
     * @return ThemeConfig instance
     */
    public static ThemeConfig getInstance() {
        if (instance == null) {
            instance = new ThemeConfig();
        }
        return instance;
    }
    
    /**
     * Sets the current theme and updates all theme-related settings.
     * 
     * @param theme theme to set
     */
    public void setTheme(ThemeMenu.Theme theme) {
        currentTheme = theme;
        updateTheme(theme);
    }
    
    private void updateTheme(ThemeMenu.Theme theme) {
        currentBrickColors = theme.getBrickColors();
        currentBackgroundImage = theme.getImagePath();
        currentMusicFile = theme.getMusicFile();
    }
    
    /**
     * Gets the current theme's music file name.
     * 
     * @return music file name
     */
    public String getMusicFile() {
        return currentMusicFile;
    }
    
    /**
     * Gets the brick color for the specified color code.
     * 
     * @param colorCode color code (1=I, 2=O, 3=T, 4=S, 5=Z, 6=J, 7=L)
     * @return paint color for the brick
     */
    public Paint getBrickColor(int colorCode) {
        int index = colorCode - 1;
        if (index >= 0 && index < currentBrickColors.length) {
            try {
                return Color.web(currentBrickColors[index]);
            } catch (Exception e) {
                return getDefaultColor(colorCode);
            }
        }
        return getDefaultColor(colorCode);
    }
    
    private Paint getDefaultColor(int colorCode) {
        switch (colorCode) {
            case 0:
                return Color.TRANSPARENT;
            case 1:
                return Color.AQUA;
            case 2:
                return Color.BLUEVIOLET;
            case 3:
                return Color.DARKGREEN;
            case 4:
                return Color.YELLOW;
            case 5:
                return Color.RED;
            case 6:
                return Color.BEIGE;
            case 7:
                return Color.BURLYWOOD;
            default:
                return Color.WHITE;
        }
    }
    
    /**
     * Gets the current theme's background image path.
     * 
     * @return background image path
     */
    public String getBackgroundImage() {
        return currentBackgroundImage;
    }
    
    /**
     * Gets the current theme.
     * 
     * @return current theme
     */
    public ThemeMenu.Theme getCurrentTheme() {
        return currentTheme;
    }
}

