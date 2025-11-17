package com.comp2042.audio;

import javafx.scene.media.AudioClip;
import javafx.scene.media.Media;
import javafx.scene.media.MediaPlayer;

import java.net.URL;

public class AudioManager {
    
    private static AudioManager instance;
    private MediaPlayer musicPlayer;
    private MediaPlayer menuMusicPlayer;
    private double masterVolume = 1.0;
    private double musicVolume = 0.4;
    private double soundEffectVolume = 1.0;
    
    private AudioManager() {
        // Private constructor for singleton
    }
    
    public static AudioManager getInstance() {
        if (instance == null) {
            instance = new AudioManager();
        }
        return instance;
    }
    
    public void playMainMenuMusic() {
        stopAllMusic();
        try {
            URL resource = getClass().getClassLoader().getResource("assets/sound/Music/main menu.wav");
            if (resource != null) {
                Media media = new Media(resource.toExternalForm());
                menuMusicPlayer = new MediaPlayer(media);
                menuMusicPlayer.setCycleCount(MediaPlayer.INDEFINITE); // Loop indefinitely
                updateMenuMusicVolume();
                menuMusicPlayer.play();
            }
        } catch (Exception e) {
            System.err.println("Error playing main menu music: " + e.getMessage());
        }
    }
    
    public void playGameMusic() {
        playGameMusic("default.mp3");
    }
    
    public void playGameMusic(String musicFile) {
        stopAllMusic();
        try {
            URL resource = getClass().getClassLoader().getResource("assets/sound/Music/" + musicFile);
            if (resource != null) {
                Media media = new Media(resource.toExternalForm());
                musicPlayer = new MediaPlayer(media);
                musicPlayer.setCycleCount(MediaPlayer.INDEFINITE); // Loop indefinitely
                updateGameMusicVolume();
                musicPlayer.play();
            }
        } catch (Exception e) {
            System.err.println("Error playing game music: " + e.getMessage());
        }
    }
    
    public void stopAllMusic() {
        if (musicPlayer != null) {
            musicPlayer.stop();
            musicPlayer = null;
        }
        if (menuMusicPlayer != null) {
            menuMusicPlayer.stop();
            menuMusicPlayer = null;
        }
    }
    
    public void pauseAllMusic() {
        if (musicPlayer != null) {
            musicPlayer.pause();
        }
        if (menuMusicPlayer != null) {
            menuMusicPlayer.pause();
        }
    }
    
    public void resumeAllMusic() {
        if (musicPlayer != null) {
            musicPlayer.play();
            updateGameMusicVolume();
        }
        if (menuMusicPlayer != null) {
            menuMusicPlayer.play();
            updateMenuMusicVolume();
        }
    }
    
    public void setMasterVolume(double volume) {
        this.masterVolume = volume / 100.0; // Convert from 0-100 to 0.0-1.0
        updateGameMusicVolume();
        updateMenuMusicVolume();
    }
    
    public void setMusicVolume(double volume) {
        this.musicVolume = volume / 100.0; // Convert from 0-100 to 0.0-1.0
        updateGameMusicVolume();
        updateMenuMusicVolume();
    }
    
    public void setSoundEffectVolume(double volume) {
        this.soundEffectVolume = volume / 100.0; // Convert from 0-100 to 0.0-1.0
    }
    
    private void updateGameMusicVolume() {
        if (musicPlayer != null) {
            musicPlayer.setVolume(masterVolume * musicVolume);
        }
    }
    
    private void updateMenuMusicVolume() {
        if (menuMusicPlayer != null) {
            menuMusicPlayer.setVolume(masterVolume * musicVolume);
        }
    }
    
    public void playSoundEffect(String soundName) {
        try {
            URL resource = getClass().getClassLoader().getResource("assets/sound/SoundEffect/" + soundName);
            if (resource != null) {
                AudioClip clip = new AudioClip(resource.toExternalForm());
                clip.setVolume(masterVolume * soundEffectVolume);
                clip.play();
            }
        } catch (Exception e) {
            System.err.println("Error playing sound effect: " + e.getMessage());
        }
    }
    
    public double getMasterVolume() {
        return masterVolume * 100.0;
    }
    
    public double getMusicVolume() {
        return musicVolume * 100.0;
    }
    
    public double getSoundEffectVolume() {
        return soundEffectVolume * 100.0;
    }
}

