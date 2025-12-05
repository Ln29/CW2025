package com.comp2042;

import com.comp2042.controller.GameController;
import com.comp2042.ui.GuiController;
import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;

import java.net.URL;

/**
 * Main entry point for the Tetris JavaFX application.
 * Initializes the UI, loads the FXML layout, and sets up the game controller.
 */
public class Main extends Application {

    /**
     * Initializes and displays the JavaFX application window.
     * 
     * @param primaryStage the primary stage for the application
     * @throws Exception if FXML loading fails
     */
    @Override
    public void start(Stage primaryStage) throws Exception {

        URL location = getClass().getClassLoader().getResource("gameLayout.fxml");
        if(location == null){
            throw new IllegalStateException("Unable to load FXML: gameLayout.fxml");
        }
        FXMLLoader fxmlLoader = new FXMLLoader(location, null);
        Parent root = fxmlLoader.load();
        GuiController c = fxmlLoader.getController();

        primaryStage.setTitle("TetrisJFX");
        Scene scene = new Scene(root, 900, 700);
        primaryStage.setScene(scene);
        primaryStage.setResizable(false);
        primaryStage.show();
        
        c.setPrimaryStage(primaryStage);
        new GameController(c);
        c.showMainMenu();
    }

    /**
     * Application entry point.
     * 
     * @param args command line arguments
     */
    public static void main(String[] args) {
        launch(args);
    }
}
