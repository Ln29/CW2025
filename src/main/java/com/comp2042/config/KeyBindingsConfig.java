package com.comp2042.config;

import javafx.scene.input.KeyCode;
import java.util.HashMap;
import java.util.Map;

/**
 * Singleton configuration for keyboard bindings.
 * Manages mapping between game actions and keyboard keys.
 */
public class KeyBindingsConfig {
    
    /**
     * Enumeration of game actions that can be bound to keys.
     */
    public enum Action {
        MOVE_LEFT,
        MOVE_RIGHT,
        ROTATE,
        SOFT_DROP,
        HARD_DROP,
        HOLD,
        PAUSE
    }
    
    private static KeyBindingsConfig instance;
    private Map<Action, KeyCode> bindings;
    
    private KeyBindingsConfig() {
        bindings = new HashMap<>();
        // Default key bindings
        bindings.put(Action.MOVE_LEFT, KeyCode.LEFT);
        bindings.put(Action.MOVE_RIGHT, KeyCode.RIGHT);
        bindings.put(Action.ROTATE, KeyCode.UP);
        bindings.put(Action.SOFT_DROP, KeyCode.DOWN);
        bindings.put(Action.HARD_DROP, KeyCode.SPACE);
        bindings.put(Action.HOLD, KeyCode.SHIFT);
        bindings.put(Action.PAUSE, KeyCode.ESCAPE);
    }
    
    /**
     * Gets the singleton KeyBindingsConfig instance.
     * 
     * @return KeyBindingsConfig instance
     */
    public static KeyBindingsConfig getInstance() {
        if (instance == null) {
            instance = new KeyBindingsConfig();
        }
        return instance;
    }
    
    /**
     * Gets the key bound to the specified action.
     * 
     * @param action game action
     * @return bound key code, or null if not bound
     */
    public KeyCode getKey(Action action) {
        return bindings.get(action);
    }
    
    /**
     * Sets the key binding for an action.
     * 
     * @param action game action
     * @param key key code to bind
     */
    public void setKey(Action action, KeyCode key) {
        bindings.put(action, key);
    }
    
    /**
     * Gets the action bound to the specified key.
     * 
     * @param key key code
     * @return bound action, or null if not bound
     */
    public Action getAction(KeyCode key) {
        for (Map.Entry<Action, KeyCode> entry : bindings.entrySet()) {
            if (entry.getValue() == key) {
                return entry.getKey();
            }
        }
        return null;
    }
    
    /**
     * Checks if a key conflicts with existing bindings.
     * 
     * @param key key code to check
     * @param excludeAction action to exclude from conflict check
     * @return true if key conflicts, false otherwise
     */
    public boolean hasConflict(KeyCode key, Action excludeAction) {
        return getConflictingAction(key, excludeAction) != null;
    }
    
    /**
     * Gets the action that conflicts with the specified key.
     * 
     * @param key key code to check
     * @param excludeAction action to exclude from conflict check
     * @return conflicting action, or null if no conflict
     */
    public Action getConflictingAction(KeyCode key, Action excludeAction) {
        for (Map.Entry<Action, KeyCode> entry : bindings.entrySet()) {
            if (entry.getKey() != excludeAction && entry.getValue() == key) {
                return entry.getKey();
            }
        }
        return null;
    }
    
    /**
     * Gets a copy of all key bindings.
     * 
     * @return map of all action-to-key bindings
     */
    public Map<Action, KeyCode> getAllBindings() {
        return new HashMap<>(bindings);
    }
}

