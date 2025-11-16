package com.comp2042.theme;

import com.comp2042.ui.menu.ThemeMenu;
import javafx.scene.paint.Color;
import javafx.scene.paint.Paint;

public class ThemeConfig {
    
    private static ThemeConfig instance;
    private ThemeMenu.Theme currentTheme = ThemeMenu.Theme.DEFAULT;
    private String[] currentBrickColors;
    private String currentBackgroundImage;
    private String currentMusicFile;
    
    private ThemeConfig() {
        updateTheme(ThemeMenu.Theme.DEFAULT);
    }
    
    public static ThemeConfig getInstance() {
        if (instance == null) {
            instance = new ThemeConfig();
        }
        return instance;
    }
    
    public void setTheme(ThemeMenu.Theme theme) {
        currentTheme = theme;
        updateTheme(theme);
    }
    
    private void updateTheme(ThemeMenu.Theme theme) {
        currentBrickColors = theme.getBrickColors();
        currentBackgroundImage = theme.getImagePath();
        currentMusicFile = theme.getMusicFile();
    }
    
    public String getMusicFile() {
        return currentMusicFile;
    }
    
    public Paint getBrickColor(int colorCode) {
        // Map color codes to brick colors: 1=I, 2=O, 3=T, 4=S, 5=Z, 6=J, 7=L
        int index = colorCode - 1;
        if (index >= 0 && index < currentBrickColors.length) {
            try {
                return Color.web(currentBrickColors[index]);
            } catch (Exception e) {
                // Fallback to default colors if parsing fails
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
    
    public String getBackgroundImage() {
        return currentBackgroundImage;
    }
    
    public ThemeMenu.Theme getCurrentTheme() {
        return currentTheme;
    }
}

