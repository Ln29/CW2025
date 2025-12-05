package com.comp2042.config;

import javafx.scene.input.KeyCode;
import java.util.HashMap;
import java.util.Map;

public class KeyBindingsConfig {
    
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
    
    public static KeyBindingsConfig getInstance() {
        if (instance == null) {
            instance = new KeyBindingsConfig();
        }
        return instance;
    }
    
    public KeyCode getKey(Action action) {
        return bindings.get(action);
    }
    
    public void setKey(Action action, KeyCode key) {
        bindings.put(action, key);
    }
    
    public Action getAction(KeyCode key) {
        for (Map.Entry<Action, KeyCode> entry : bindings.entrySet()) {
            if (entry.getValue() == key) {
                return entry.getKey();
            }
        }
        return null;
    }
    
    public boolean hasConflict(KeyCode key, Action excludeAction) {
        return getConflictingAction(key, excludeAction) != null;
    }
    
    public Action getConflictingAction(KeyCode key, Action excludeAction) {
        for (Map.Entry<Action, KeyCode> entry : bindings.entrySet()) {
            if (entry.getKey() != excludeAction && entry.getValue() == key) {
                return entry.getKey();
            }
        }
        return null;
    }
    
    public Map<Action, KeyCode> getAllBindings() {
        return new HashMap<>(bindings);
    }
}

