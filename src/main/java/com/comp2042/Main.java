package com.comp2042;

import com.comp2042.controller.GameController;
import com.comp2042.ui.GuiController;
import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;

import java.net.URL;

public class Main extends Application {

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
        
        // Set the primary stage in GuiController for exit functionality
        c.setPrimaryStage(primaryStage);
        
        // Initialize the game controller (game will start in paused state)
        new GameController(c);
        
        // Show the main menu
        c.showMainMenu();
    }


    public static void main(String[] args) {
        launch(args);
    }
}
