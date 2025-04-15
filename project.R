# Load necessary libraries
install.packages("shiny")
library(shiny)
install.packages("caret",dependencies = TRUE)
library(caret)
install.packages("class")
library(class)
install.packages("rpart")
library(rpart)
install.packages("e1071")
library(e1071)   
install.packages("neuralnet")
library(neuralnet)
install.packages("ggplot2")
library(ggplot2)

# Load the dataset
data(iris)

# Split the data into training and testing sets (80% train, 20% test)
set.seed(123)
trainIndex <- createDataPartition(iris$Species, p = 0.8, list = FALSE)
trainData <- iris[trainIndex, ]
testData <- iris[-trainIndex, ]

# Function to calculate accuracy for each algorithm
calculate_accuracies <- function() {
  
  # KNN Model
  knn_model <- knn(trainData[, -5], testData[, -5], trainData$Species, k = 3)
  knn_accuracy <- mean(knn_model == testData$Species) * 100
  
  # Decision Tree Model
  dt_model <- rpart(Species ~ ., data = trainData)
  dt_predictions <- predict(dt_model, testData, type = "class")
  dt_accuracy <- mean(dt_predictions == testData$Species) * 100
  
  # Naive Bayes Model
  nb_model <- naiveBayes(Species ~ ., data = trainData)
  nb_predictions <- predict(nb_model, testData)
  nb_accuracy <- mean(nb_predictions == testData$Species) * 100
  
  # ANN Model
  nn_model <- neuralnet(Species ~ Sepal.Length + Sepal.Width + 
                          Petal.Length + Petal.Width,
                        data = trainData, hidden = c(5, 3), 
                        linear.output = FALSE)
  nn_predictions <- compute(nn_model, testData[, -5])$net.result
  nn_accuracy <- mean(max.col(nn_predictions) == as.numeric(testData$Species)) * 100
  
  # Return a data frame with all accuracies
  data.frame(
    Model = c("KNN", "Decision Tree", "Naive Bayes", "ANN"),
    Accuracy = c(knn_accuracy, dt_accuracy, nb_accuracy, nn_accuracy)
  )
}

# Store accuracies for display
accuracy_df <- calculate_accuracies()

# Define UI for Shiny App
ui <- fluidPage(
  titlePanel("Predictive Analysis Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("algorithm", "Select Algorithm:",
                  choices = accuracy_df$Model)
    ),
    
    mainPanel(
      plotOutput("accuracyPlot"),
      textOutput("accuracyText")
    )
  )
)

# Define Server Logic for Shiny App
server <- function(input, output) {
  
  # Render the accuracy comparison plot
  output$accuracyPlot <- renderPlot({
    ggplot(accuracy_df, aes(x = Model, y = Accuracy, fill = Model)) +
      geom_bar(stat = "identity", width = 0.6) +
      theme_minimal() +
      labs(title = "Model Accuracy Comparison", 
           y = "Accuracy (%)", x = "Model")
  })
  
  # Display the accuracy of the selected algorithm
  output$accuracyText <- renderText({
    selected_accuracy <- accuracy_df$Accuracy[accuracy_df$Model == input$algorithm]
    paste("Accuracy for", input$algorithm, ":", round(selected_accuracy, 2), "%")
  })
}

# Run the Shiny App
shinyApp(ui = ui, server = server)
