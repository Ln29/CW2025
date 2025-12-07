# Tetris Game - Coursework Documentation

## GitHub Repository
https://github.com/Ln29/CW2025

## Compilation Instructions

### Prerequisites
- Java Development Kit (JDK) 23
- Apache Maven 3.6+ (or use the included Maven wrapper)

### Building the Project

1. **Using Maven directly:**
   ```bash
   mvn clean compile
   ```

2. **Using Maven Wrapper:**
   ```bash
   # On Windows
   mvnw.cmd clean compile
   
   # On Linux/Mac
   ./mvnw clean compile
   ```

### Running the Application

1. **Using Maven:**
   ```bash
   mvn javafx:run
   ```

2. **Using Maven Wrapper:**
   ```bash
   # On Windows
   mvnw.cmd javafx:run
   
   # On Linux/Mac
   ./mvnw javafx:run
   ```

### Dependencies
The project uses Maven to manage dependencies. Key dependencies include:
- JavaFX Controls (21.0.6)
- JavaFX FXML (21.0.6)
- JavaFX Media (21.0.6)
- JUnit Jupiter (5.12.1) - for testing

All dependencies are automatically downloaded by Maven during the first build.

---

## Implemented and Working Properly

### Core Game Features
1. **Basic Tetris Gameplay**
   - All 7 standard Tetris bricks (I, O, T, S, Z, J, L)
   - Brick movement (left, right, down)
   - Brick rotation with wall kick mechanics (clockwise)
   - Automatic brick falling
   - Line clearing with scoring when rows are filled
   - Lock delay mechanism for brick adjustments
   - Hard drop functionality
   - Ghost brick preview showing where the brick will land
   - Game over detection (win, lose)

2. **Hold Feature**
   - Hold current brick for later use
   - Visual display of held brick
   - One hold per brick placement

3. **Next Brick Preview**
   - Display of 5 upcoming bricks

### Game Modes
1. **Endless Mode**
   - Play indefinitely (999 lines target)
   - Constant speed based on difficulty level (0-10)
   - Mode-specific high score tracking

2. **Marathon Mode**
   - Increasing speed based on difficulty level (0-10) as lines are cleared
   - Configurable target lines
   - Speed progression based on lines cleared
   - Mode-specific and target line-specific high score tracking

3. **Survival Mode**
   - Garbage rows spawn periodically
   - Increasing speed over time
   - Must clear target lines while managing garbage
   - Configurable difficulty and spawn intervals
   - Mode-specific and difficulty-specific high score tracking

### UI Features
1. **Keyboard Navigation**
   - Support for both mouse clicks and keyboard input throughout the UI

2. **Main Menu**
   - Start game option
   - Settings access
   - Exit option

3. **Settings Menu**
   - Volume controls (master, music, sound effects)
   - Key bindings customization
   - Theme selection
   - Back navigation

4. **Key Bindings Menu**
   - Singleton configuration manager
   - Customize all game controls
   - Conflict detection for key bindings
   - Visual feedback for key assignment
   - Save and apply bindings

5. **Theme Selection**
   - Singleton theme manager
   - Multiple visual themes (Default, Beach, Party, Snowy Mountain)
   - Theme-specific background images
   - Theme-specific music tracks
   - Theme-specific brick colors

6. **Mode Selection Menu**
   - Choose game mode before starting
   - Configure mode-specific settings (difficulty, target lines, etc.)
   - Visual mode descriptions

7. **Pause Menu**
   - Resume game
   - Restart game
   - Return to main menu

8. **Game Over Menu**
   - Restart option
   - Return to main menu

9. **In-Game Panels**
   - Next brick panel (shows upcoming bricks)
   - Hold brick panel (shows held brick)
   - Stats panels (left and right)
     - Game mode display
     - Time elapsed
     - Current level
     - High score
     - Target lines
     - Current score

10. **Notifications**
   - Combo notifications (when clearing multiple lines consecutively)
   - Score bonus notifications
   - Level up notifications

### Audio Features
1. **Music System**
   - Main menu music
   - Game music (theme-specific)
   - Music volume control
   - Music pause/resume

2. **Sound Effects**
   - Line clear sound
   - Block fall sound
   - Click sounds
   - Win/lose sounds
   - Sound effect volume control

### Code Quality Features
1. **Comprehensive Javadoc Documentation**
   - All public classes documented
   - All public methods documented
   - Parameter and return value documentation
   - Ready for HTML generation

2. **Code Refactoring**
   - Eliminated code duplication
   - Extracted helper methods
   - Improved code organization
   - Enhanced maintainability

---

## Implemented but Not Working Properly

1. **Ghost Brick Delay in Survival Mode**
   - **Description:** The ghost brick is implemented but experiences a delay when rows are pushed up by garbage lines in Survival Mode.
   - **Steps Taken:**
     - Investigated the rendering and update order in the game loop
     - Attempted to refresh the ghost brick immediately after row pushes, but timing conflicts with the garbage push animation remain unresolved
   - **Future Work:** Optimize the update logic to ensure the ghost brick refreshes instantly after garbage pushes.

2. **Overlapping of Last Row During Game Over**
   - **Description:** The last row of bricks sometimes overlaps with bricks outside the visible game board when the game ends.
   - **Steps Taken:**
     - Checked the collision and boundary detection logic
     - Tried clamping brick positions to the board limits, but edge cases during fast row drops were not fully resolved
   - **Future Work:**
     - Refine the game-over logic to ensure all bricks are confined within the board
     - Implement a smooth clearing animation for the last row to avoid visual glitches
---

## Features Not Implemented

1. **Highscore Board**
   - **Description:** A highscore board for each game mode, with separate score tracking for each difficulty level.
   - **Reason:**
     - Not implemented due to limited development time
     - Would require additional data structures, persistent storage, and UI components that would significantly expand the project
   - **Future Work:** Implement a persistent highscore system to enhance replayability and give players long-term goals across different modes and difficulty levels.

2. **Multiplayer Mode**
   - **Description:** A multiplayer system allowing two players to play cooperatively or competitively.
   - **Reason:**
     - Not implemented because of time constraints
     - Would make the overall system larger than necessary for this coursework (already implement gamemode for single player)
   - **Future Work:** Add local or online multiplayer to increase engagement, variety, and replay value.

## Comparison: Base Game vs Current Version

### Package Structure Changes

**Base Game Structure:**
- `com.comp2042` - Main package (18 classes)
- `com.comp2042.logic.bricks` - Brick implementations (10 classes)
- Total: 28 classes

**Current Version Structure:**
- `com.comp2042` - Main entry point
- `com.comp2042.config` - Configuration classes (4 classes)
- `com.comp2042.controller` - Game control logic (5 classes)
- `com.comp2042.core` - Core game logic (10 classes)
  - `com.comp2042.core.bricks` - Brick implementations (4 classes)
  - `com.comp2042.core.mode` - Game mode implementations (5 classes)
- `com.comp2042.input` - Input handling (6 classes)
- `com.comp2042.ui` - User interface (8 classes)
  - `com.comp2042.ui.menu` - Menu components (11 classes)
  - `com.comp2042.ui.panels` - UI panels (8 classes)
  - `com.comp2042.ui.util` - UI utilities (5 classes)
- Total: 67 classes

### Key Improvements

1. **Better Package Organization**
   - Separated concerns into logical packages
   - Clear separation between UI, core logic, and configuration
   - Improved code navigation and maintainability

2. **Enhanced Functionality**
   - Added multiple game modes (Endless, Marathon, Survival)
   - Added theme system with multiple themes
   - Added comprehensive menu system
   - Added key bindings customization
   - Added hold feature
   - Added ghost brick preview
   - Added notification system
   - Added audio management system
   - Added wall kick and lock delay mechanism

3. **Code Quality Improvements**
   - Extensive refactoring to eliminate duplication
   - Added comprehensive Javadoc documentation
   - Improved error handling

---

## New Java Classes
### Configuration Package (`com.comp2042.config`)
1. **GameConstants.java** - Centralized constants for game configuration (board dimensions, brick size, hidden rows, etc.)
2. **GameModeConfig.java** - Configuration manager for game modes (Endless, Marathon, Survival) with difficulty settings
3. **KeyBindingsConfig.java** - Singleton configuration manager for customizable keyboard controls with conflict detection
4. **ThemeConfig.java** - Singleton theme manager supporting multiple visual themes (Default, Beach, Party, Snowy Mountain)

### Controller Package (`com.comp2042.controller`)
5. **AudioManager.java** - Manages music and sound effects playback with separate volume controls
6. **GameLifecycle.java** - Handles game timing, speed updates, and automatic brick falling
7. **GameModeController.java** - Controls game mode-specific logic (speed calculation, garbage spawning, win/lose conditions)
8. **GameState.java** - Tracks game state (paused, game over, time elapsed, level progression)

### Core Package - Game Modes (`com.comp2042.core.mode`)
9. **GameMode.java** - Abstract base class defining the game mode interface
10. **GameModeStrategy.java** - Strategy interface for different game mode behaviors
11. **EndlessMode.java** - Implementation for endless mode with constant speed
12. **MarathonMode.java** - Implementation for marathon mode with increasing speed based on lines cleared
13. **SurvivalMode.java** - Implementation for survival mode with garbage row spawning

### Core Package (`com.comp2042.core`)
14. **GhostBrick.java** - Implements ghost brick preview showing where the current brick will land

### Input Package (`com.comp2042.input`)
15. **InputHandler.java** - Centralized input handling with key binding support and event routing
16. **InputRouter.java** - Routes input events to appropriate handlers based on game state (menu vs gameplay)

### UI Package - Rendering (`com.comp2042.ui`)
17. **GameRenderer.java** - Handles rendering of game board, current brick, and game elements
18. **GhostRenderer.java** - Specialized renderer for ghost brick visualization
19. **GridRenderer.java** - Renders the game grid and board matrix
20. **LineClearHandler.java** - Manages line clearing animations and effects

### UI Package - Menus (`com.comp2042.ui.menu`)
21. **MenuCallbacks.java** - Interface defining callbacks for menu actions
22. **MenuController.java** - Central controller managing menu navigation and transitions
23. **MenuFactory.java** - Factory for creating menu instances
24. **MenuManager.java** - Manages menu lifecycle and state
25. **MainMenu.java** - Main menu UI implementation
26. **SettingsMenu.java** - Settings menu with volume and theme controls
27. **KeyBindingsMenu.java** - Menu for customizing keyboard controls
28. **ThemeMenu.java** - Menu for selecting visual themes
29. **ModeSelectionMenu.java** - Menu for choosing game mode and configuring mode settings
30. **PauseMenu.java** - In-game pause menu
31. **GameOverMenu.java** - Game over screen with restart options

### UI Package - Panels (`com.comp2042.ui.panels`)
32. **PanelManager.java** - Manages all UI panels (next bricks, hold, stats)
33. **PanelPositioner.java** - Handles panel positioning and layout
34. **NextBrickPanel.java** - Displays upcoming bricks preview
35. **HoldBrickPanel.java** - Displays held brick
36. **StatsPanel.java** - Left stats panel showing game information
37. **RightStatsPanel.java** - Right stats panel with additional game metrics
38. **NotificationPanelManager.java** - Manages notification display and animations

### UI Package - Utilities (`com.comp2042.ui.util`)
39. **LayoutHelper.java** - Utility methods for UI layout calculations
40. **MenuNavigationHandler.java** - Handles keyboard navigation in menus
41. **NavigationInput.java** - Input handling utilities for menu navigation
42. **PlatformUtils.java** - Platform-specific utility methods
43. **SceneAccessor.java** - Utility for accessing JavaFX scene elements

### UI Package (`com.comp2042.ui`)
44. **GameStateManager.java** - Manages game state transitions and lifecycle
45. **ThemeApplier.java** - Applies theme settings to UI elements
46. **NotificationService.java** - Service for displaying game notifications (combos, level ups, etc.)

## Modified Java Classes

### Core Package (`com.comp2042.core`)
1. **Board.java** - Added Javadoc documentation; interface remains functionally similar but moved to core package
2. **BrickRotator.java** - Enhanced with wall kick mechanics for better rotation behavior; added comprehensive Javadoc
3. **Brick.java** - Moved from `logic.bricks` to `core.bricks`; added Javadoc documentation; interface structure unchanged
4. **BrickGenerator.java** - Moved from `logic.bricks` to `core.bricks`; interface enhanced with Javadoc; now supports multiple generator implementations
5. **BrickFactory.java** - Refactored from individual brick classes (IBrick.java, JBrick.java, LBrick.java, OBrick.java, SBrick.java, TBrick.java, ZBrick.java) into a single factory class with nested classes; consolidates all 7 brick types for better code organization and maintainability
6. **RandomBrickGenerator.java** → **SevenBagBrickGenerator.java** - Replaced random brick generation with 7-bag system for better brick distribution and more balanced gameplay; implements the same `BrickGenerator` interface
7. **ClearRow.java** - Added Javadoc; functionality enhanced to support multiple game modes
8. **DownData.java** - Added Javadoc documentation; structure remains similar
9. **MatrixOperations.java** - Significantly refactored:
   - Replaced Deque-based row clearing with more efficient two-pass algorithm
   - Added helper methods (`forEachBrickCell`, `forEachBrickCellWithValue`) to eliminate code duplication
   - Improved performance with early exits and reduced temporary data structures
   - Added comprehensive Javadoc
10. **NextShapeInfo.java** - Enhanced to support multiple next bricks (5-brick preview); added Javadoc
11. **Score.java** - Enhanced with mode-specific high score tracking; added Javadoc
12. **SimpleBoard.java** - Major modifications:
    - Added hold feature with `holdBrick()` method and `holdUsed` flag
    - Implemented lock delay mechanism (500ms) for brick adjustments
    - Switched from `RandomBrickGenerator` to `SevenBagBrickGenerator` for better brick distribution
    - Fixed matrix dimensions (was `[width][height]`, now `[height][width]`)
    - Added comprehensive Javadoc
    - Refactored movement methods to eliminate duplication
    - Enhanced `holdBrick()` to prevent duplicate logic
13. **ViewData.java** - Enhanced to support ghost brick data; added Javadoc

### Input Package (`com.comp2042.input`)
14. **EventSource.java** - Moved from base package to input package; added Javadoc; enum values remain similar
15. **EventType.java** - Moved from base package to input package; added Javadoc; enum structure unchanged
16. **InputEventListener.java** - Moved from base package to input package; interface enhanced with Javadoc
17. **MoveEvent.java** - Moved from base package to input package; added Javadoc; structure remains similar

### Controller Package (`com.comp2042.controller`)
18. **GameController.java** - Significant refactoring:
    - Moved from base package to controller package
    - Enhanced to work with new game mode system
    - Added lock delay handling
    - Improved error handling with null checks
    - Added comprehensive Javadoc
    - Now coordinates with `GameModeController` for mode-specific behavior

### UI Package (`com.comp2042.ui`)
19. **GuiController.java** - Extensive modifications:
    - Complete architectural overhaul with separation of concerns
    - Integrated menu system (`MenuController`, `MenuManager`)
    - Added theme support via `ThemeApplier`
    - Integrated audio system via `AudioManager`
    - Added game mode support via `GameModeController`
    - Separated rendering logic into `GameRenderer`, `GhostRenderer`, `GridRenderer`
    - Added panel management via `PanelManager`
    - Integrated notification system via `NotificationPanelManager`
    - Added input routing via `InputHandler` and `InputRouter`
    - Extracted helper methods to eliminate code duplication (null-check helpers)
    - Added comprehensive Javadoc
    - Enhanced with pause/resume functionality
    - Integrated game state management

### Main Package (`com.comp2042`)
20. **Main.java** - Enhanced to initialize new configuration systems (themes, key bindings); added Javadoc

### UI Package - Panels (`com.comp2042.ui.panels`)
21. **NotificationPanel.java** - Moved from base package to `ui.panels` package; enhanced with better animation support and integration with `NotificationPanelManager`

## Unexpected Problems

### 1. Brick Stuck in Garbage in Survival Mode
- **Problem:** During Survival Mode, when garbage rows rise from the bottom, sometimes the falling brick would get "stuck" inside the garbage instead of landing correctly. This occurred when the brick's position conflicted with the rising garbage rows, causing the collision detection to incorrectly identify the brick as being in a valid position when it should have been pushed up or repositioned.
- **Root Cause:** The original `addGarbageRowsFromBottom()` implementation did not properly handle the interaction between the active falling brick and incoming garbage rows. When garbage rows were added, the brick's position was not adjusted, leading to situations where the brick would appear to be inside the garbage blocks.
- **Solution Implemented:**
  - **Collision detection before garbage insertion:** The solution checks if the active brick's lowest row intersects with the garbage spawn position before adding garbage rows (lines 399-423 in `SimpleBoard.java`).
  - **Automatic brick locking:** If the brick's lowest row is at or below the garbage spawn position (`brickLowestRow >= garbageSpawnPosition`), the brick is immediately locked to the background before garbage rows are added, preventing it from getting stuck.
  - **Brick position adjustment:** For bricks that don't need to be locked, the solution moves the brick up by the number of garbage rows being added (`newBrickY = currentOffset.getY() - numRowsToAdd`), ensuring the brick maintains its relative position above the garbage.
  - **Game over detection:** The solution checks if the brick is pushed out of bounds (above row 0) after adjustment, which triggers game over if the garbage push is too aggressive.
  - **Proper state management:** The solution resets the lock delay timer and brick rotator state when a brick is locked due to garbage collision.
- **Result:** The game now properly handles garbage row insertion in Survival Mode. Bricks are either locked before garbage arrives or repositioned correctly above the garbage, preventing the "stuck brick" issue. The collision detection ensures smooth gameplay even when multiple garbage rows spawn rapidly.

### 2. UI Overlap
- **Problem:** UI elements such as the Game Over menu were overlapping with gameplay elements like the ghost brick, creating visual conflicts where menu overlays appeared on top of game rendering elements. This made the interface confusing and potentially obscured important game information.
- **Root Cause:** The original implementation did not have a clear separation between gameplay rendering and UI overlays. All elements were rendered in the same layer, causing z-order conflicts where menus and gameplay elements could overlap unpredictably.
- **Solution Implemented:**
  - **Separated rendering layers:** Created two distinct rendering layers in `GuiController.createRenderingLayers()`:
    - `gameplayLayer` (viewOrder 1.0): Contains all gameplay elements (game board, brick panel, ghost brick, notification panels)
    - `uiLayer` (viewOrder 0.0): Contains all menu overlays (main menu, pause menu, game over menu, settings, etc.)
  - **ViewOrder system:** JavaFX's `setViewOrder()` ensures that lower values render on top. The `uiLayer` with viewOrder 0.0 always renders above the `gameplayLayer` with viewOrder 1.0.
  - **MenuManager implementation:** Created `MenuManager` class to centralize menu display logic. The `ensureOnTop()` method ensures menus are always moved to the top of the `uiLayer` when shown.
  - **Menu viewOrder:** All menu classes (MainMenu, PauseMenu, GameOverMenu, etc.) use `setViewOrder(-1)` to ensure they render above other UI elements.
  - **Layer visibility control:** The `gameplayLayer` visibility is controlled when showing/hiding menus, and the `uiLayer` always remains visible for menu navigation.
- **Result:** The UI now has clear visual separation between gameplay and menus. Menus always render on top of gameplay elements, eliminating overlap issues. The layered architecture makes it easy to manage z-order and ensures consistent rendering behavior across all menu transitions.

### 3. Game Does Not Pause Properly
- **Problem:** The game's pause functionality was inconsistent; returning to the main menu did not fully stop ongoing gameplay, allowing bricks to continue falling even when the main menu was displayed. This occurred because the game timers (brick movement, lock delay checks, and game time tracking) were not being properly stopped when navigating away from gameplay.
- **Root Cause:** The original implementation lacked a centralized state management system. When returning to the main menu, the code was not explicitly stopping all game timers, which meant the automatic brick falling continued in the background.
- **Solution Implemented:**
  - **Created `GameStateManager` class:** Extracted game state management logic from `GuiController` into a dedicated `GameStateManager` class to centralize pause, resume, restart, and menu navigation logic.
  - **Implemented proper timer stopping:** The `returnToMainMenu()` method in `GameStateManager` now explicitly calls `gameLifecycle.stopTimers()` to completely stop all three game timers (brick movement timeline, lock delay check timeline, and game time timeline) instead of just pausing them.
  - **Added comprehensive state reset:** The solution ensures that when returning to the main menu:
    - All game timers are stopped via `GameLifecycle.stopTimers()` which calls `controlTimers(false)` to stop all timeline animations
    - The game mode controller is reset
    - The gameplay layer is hidden (`gameplayLayer.setVisible(false)`)
    - All menus are properly hidden and the main menu is shown
    - The pause state is set to `TRUE` to prevent any accidental resumption
    - Main menu music is played
  - **Enhanced `GameLifecycle` class:** The `GameLifecycle` class provides clear separation between `pauseTimers()` (temporarily pauses but keeps timers alive) and `stopTimers()` (completely stops and clears timers), ensuring the correct method is used for menu navigation versus in-game pausing.
- **Result:** The game now properly stops all gameplay when returning to the main menu, preventing bricks from continuing to fall. The centralized state management also makes it easier to maintain consistent behavior across all menu transitions.

---