package com.comp2042.controller;

import javafx.scene.media.AudioClip;
import javafx.scene.media.Media;
import javafx.scene.media.MediaPlayer;

import java.net.URL;

/**
 * Singleton manager for game audio including music and sound effects.
 * Handles volume control, playback, pause, and resume functionality.
 */
public class AudioManager {
    
    private static AudioManager instance;
    private MediaPlayer musicPlayer;
    private MediaPlayer menuMusicPlayer;
    private double masterVolume = 1.0;
    private double musicVolume = 0.4;
    private double soundEffectVolume = 1.0;
    
    private AudioManager() {
    }
    
    /**
     * Gets the singleton AudioManager instance.
     * 
     * @return AudioManager instance
     */
    public static AudioManager getInstance() {
        if (instance == null) {
            instance = new AudioManager();
        }
        return instance;
    }
    
    /**
     * Plays the main menu background music (loops indefinitely).
     */
    public void playMainMenuMusic() {
        stopAllMusic();
        try {
            URL resource = getClass().getClassLoader().getResource("assets/sound/Music/main menu.mp3");
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
    
    /**
     * Plays the default game music.
     */
    public void playGameMusic() {
        playGameMusic("default.mp3");
    }
    
    /**
     * Plays the specified game music file (loops indefinitely).
     * 
     * @param musicFile name of the music file in assets/sound/Music/
     */
    public void playGameMusic(String musicFile) {
        stopAllMusic();
        try {
            URL resource = getClass().getClassLoader().getResource("assets/sound/Music/" + musicFile);
            if (resource != null) {
                Media media = new Media(resource.toExternalForm());
                musicPlayer = new MediaPlayer(media);
                musicPlayer.setCycleCount(MediaPlayer.INDEFINITE); 
                updateGameMusicVolume();
                musicPlayer.play();
            }else {
                System.err.println("Error: Music file not found: assets/sound/Music/" + musicFile);
            }
        } catch (Exception e) {
            System.err.println("Error playing game music: " + e.getMessage());
        }
    }
    
    /**
     * Stops all currently playing music.
     */
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
    
    /**
     * Pauses all currently playing music.
     */
    public void pauseAllMusic() {
        if (musicPlayer != null) {
            musicPlayer.pause();
        }
        if (menuMusicPlayer != null) {
            menuMusicPlayer.pause();
        }
    }
    
    /**
     * Resumes all paused music.
     */
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
    
    /**
     * Sets the master volume (0-100).
     * 
     * @param volume volume level (0-100)
     */
    public void setMasterVolume(double volume) {
        this.masterVolume = volume / 100.0; 
        updateGameMusicVolume();
        updateMenuMusicVolume();
    }
    
    /**
     * Sets the music volume (0-100).
     * 
     * @param volume volume level (0-100)
     */
    public void setMusicVolume(double volume) {
        this.musicVolume = volume / 100.0; 
        updateGameMusicVolume();
        updateMenuMusicVolume();
    }
    
    /**
     * Sets the sound effect volume (0-100).
     * 
     * @param volume volume level (0-100)
     */
    public void setSoundEffectVolume(double volume) {
        this.soundEffectVolume = volume / 100.0; 
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
    
    /**
     * Plays a sound effect once.
     * 
     * @param soundName name of the sound file in assets/sound/SoundEffect/
     */
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
    
    /**
     * Gets the master volume (0-100).
     * 
     * @return master volume level
     */
    public double getMasterVolume() {
        return masterVolume * 100.0;
    }
    
    /**
     * Gets the music volume (0-100).
     * 
     * @return music volume level
     */
    public double getMusicVolume() {
        return musicVolume * 100.0;
    }
    
    /**
     * Gets the sound effect volume (0-100).
     * 
     * @return sound effect volume level
     */
    public double getSoundEffectVolume() {
        return soundEffectVolume * 100.0;
    }
}

