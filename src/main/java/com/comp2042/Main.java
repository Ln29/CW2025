package com.comp2042;

import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;

import java.net.URL;
import java.util.ResourceBundle;

public class Main extends Application {

    @Override
    public void start(Stage primaryStage) throws Exception {

        URL location = getClass().getClassLoader().getResource("gameLayout.fxml");
        if(location == null){
            throw new IllegalAccessException("Unable to load FXML: gameLayout.fxml");
        }
        FXMLLoader fxmlLoader = new FXMLLoader(location,(ResourceBundle) null);
        Parent root = fxmlLoader.load();
        GuiController c = fxmlLoader.getController();

        primaryStage.setTitle("TetrisJFX");
        Scene scene = new Scene(root, 800, 700);
        primaryStage.setScene(scene);
        primaryStage.setResizable(false);
        primaryStage.show();
        c.setPrimaryStage(primaryStage);
        new GameController(c);
        c.showMainMenu();
    }


    public static void main(String[] args) {
        launch(args);
    }
}
