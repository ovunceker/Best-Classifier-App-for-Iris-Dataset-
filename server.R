library(shiny)
library(caret)
library(plotly)
library(kernlab)
library(rpart)
library(randomForest)


function(input, output, session) {
  set.seed(31)
  new_iris <- iris[,-c(2,3)]
  inTrain <- createDataPartition(new_iris$Species, p=0.75, list=FALSE)
  training <- new_iris[inTrain,]
  testing <- new_iris[-inTrain,]
  
  # Use eventReactive for all model training to wait for submit button
  svm_lin <- eventReactive(input$submit, {
    req(input$choice == "Linear SVM")
    req(input$sliderSVMlin)
    train(Species~.,data=training, method="svmLinear",
          tuneGrid = expand.grid(C = as.numeric(input$sliderSVMlin)))
  }, ignoreNULL = FALSE) 
  
  svm_rad <- eventReactive(input$submit, {
    req(input$choice == "Radial SVM")
    req(input$sliderSVMrad)
    train(Species~.,data=training, method="svmRadial",
          tuneGrid = expand.grid(sigma = 0.1,C = as.numeric(input$sliderSVMrad)))
  }, ignoreNULL = FALSE)
  
  tree <- eventReactive(input$submit, {
    req(input$choice == "Decision Tree")
    req(input$sliderTree)
    train(Species~.,data=training, method="rpart",
          tuneGrid = expand.grid(cp = as.numeric(input$sliderTree)))
  }, ignoreNULL = FALSE) 
  
  forest <- eventReactive(input$submit, {
    req(input$choice == "Random Forest")
    req(input$sliderForest1, input$sliderForest2)
    train(Species~.,data=training, method="rf",
          tuneGrid = expand.grid(mtry = as.numeric(input$sliderForest1)),
          ntree = as.numeric(input$sliderForest2))
  }, ignoreNULL = FALSE) 
  
  lda <- eventReactive(input$submit, { 
    req(input$choice == "LDA")
    train(Species~.,data=training, method="lda") 
  }, ignoreNULL = FALSE)
  
  qda <- eventReactive(input$submit, { 
    req(input$choice == "QDA")
    train(Species~.,data=training, method="qda") 
  }, ignoreNULL = FALSE)
  
  # Use eventReactive for predictions as well
  svm_lin_pred <- eventReactive(input$submit, { 
    req(input$choice == "Linear SVM")
    req(input$sliderSVMlin)
    predict(svm_lin(), testing) 
  }, ignoreNULL = FALSE)
  
  svm_rad_pred <- eventReactive(input$submit, { 
    req(input$choice == "Radial SVM")
    req(input$sliderSVMrad)
    predict(svm_rad(), testing) 
  }, ignoreNULL = FALSE)
  
  tree_pred <- eventReactive(input$submit, { 
    req(input$choice == "Decision Tree")
    req(input$sliderTree)
    predict(tree(), testing) 
  }, ignoreNULL = FALSE)
  
  forest_pred <- eventReactive(input$submit, {
    req(input$choice == "Random Forest")
    req(input$sliderForest1, input$sliderForest2)
    predict(forest(), testing) 
  }, ignoreNULL = FALSE)
  
  lda_pred <- eventReactive(input$submit, { 
    req(input$choice == "LDA")
    predict(lda(), testing) 
  }, ignoreNULL = FALSE)
  
  qda_pred <- eventReactive(input$submit, {
    req(input$choice == "QDA")
    predict(qda(), testing) 
  }, ignoreNULL = FALSE)
  
  # Get current predictions based on selected method
  current_pred <- eventReactive(input$submit, {
    req(input$choice)
    
    switch(input$choice,
           "Linear SVM" = svm_lin_pred(),
           "Radial SVM" = svm_rad_pred(),
           "Decision Tree" = tree_pred(),
           "Random Forest" = forest_pred(),
           "LDA" = lda_pred(),
           "QDA" = qda_pred())
  }, ignoreNULL = FALSE)
  
  output$plot <- renderPlotly({
    # Only run when submit button is clicked and we have valid predictions
    req(input$submit, current_pred())
    
    predictions <- current_pred()
    
    # Ensure predictions are available and have the correct length
    if (length(predictions) == 0 || length(predictions) != nrow(testing)) {
      return(plotly_empty() %>%
               layout(title = "Please select a classification method and click Submit"))
    }
    
    # Define colors and shapes
    actual_colors <- c('setosa' = '#FF6B6B', 'versicolor' = '#3CB44B', 'virginica' = '#4363D8')
    pred_shapes <- c('setosa' = 'circle', 'versicolor' = 'square', 'virginica' = 'diamond')
    
    plot_data <- data.frame(
      Petal.Width = testing$Petal.Width,
      Sepal.Length = testing$Sepal.Length,
      Species = testing$Species,
      Predicted = predictions
    )
    
    p <- plot_ly(plot_data, x = ~Petal.Width, y = ~Sepal.Length) %>%
      add_markers(
        color = ~Species,
        colors = actual_colors,
        symbol = ~Predicted,
        symbols = pred_shapes,
        opacity = 0.8,
        marker = list(size = 10),
        hoverinfo = 'text',
        text = ~paste('Actual:', Species, '<br>Predicted:', Predicted),
        showlegend = FALSE
      ) %>%
      layout(
        title = paste('Classification Results (', input$choice, ')'),
        xaxis = list(title = 'Petal Width'),
        yaxis = list(title = 'Sepal Length'),
        annotations = list(
          # Actual classes legend (colors)
          list(x = 0.02, y = 0.98, xref = "paper", yref = "paper",
               text = "<b>Actual Species (Color):</b>", showarrow = FALSE,
               xanchor = "left", font = list(size = 12)),
          list(x = 0.02, y = 0.94, xref = "paper", yref = "paper",
               text = "● setosa", showarrow = FALSE, 
               xanchor = "left", font = list(color = '#FF6B6B', size = 11)),
          list(x = 0.02, y = 0.90, xref = "paper", yref = "paper",
               text = "● versicolor", showarrow = FALSE,
               xanchor = "left", font = list(color = '#3CB44B', size = 11)),
          list(x = 0.02, y = 0.86, xref = "paper", yref = "paper",
               text = "● virginica", showarrow = FALSE,
               xanchor = "left", font = list(color = '#4363D8', size = 11)),
          
          # Predicted classes legend (shapes)
          list(x = 0.02, y = 0.78, xref = "paper", yref = "paper",
               text = "<b>Predicted Species (Shape):</b>", showarrow = FALSE,
               xanchor = "left", font = list(size = 12)),
          list(x = 0.02, y = 0.74, xref = "paper", yref = "paper",
               text = "○ setosa", showarrow = FALSE,
               xanchor = "left", font = list(size = 11)),
          list(x = 0.02, y = 0.71, xref = "paper", yref = "paper",
               text = "□ versicolor", showarrow = FALSE,
               xanchor = "left", font = list(size = 11)),
          list(x = 0.02, y = 0.64, xref = "paper", yref = "paper",
               text = "◇ virginica", showarrow = FALSE,
               xanchor = "left", font = list(size = 11))
        )
      )
    
    p
  })
  
  output$predictions <- renderText({
    req(input$submit)
    predictions <- switch(input$choice,
                          "Linear SVM" = svm_lin_pred(),
                          "Radial SVM" = svm_rad_pred(),
                          "Decision Tree" = tree_pred(),
                          "Random Forest" = forest_pred(),
                          "LDA" = lda_pred(),
                          "QDA" = qda_pred())
    cm <- confusionMatrix(testing$Species, predictions)
    accuracy <- round(cm$overall['Accuracy'], 4)
    percent <- accuracy*100
    paste(percent, "%")
    
  })
  
  output$missclassified <- renderText({
    req(input$submit)
    predictions <- switch(input$choice,
                          "Linear SVM" = svm_lin_pred(),
                          "Radial SVM" = svm_rad_pred(),
                          "Decision Tree" = tree_pred(),
                          "Random Forest" = forest_pred(),
                          "LDA" = lda_pred(),
                          "QDA" = qda_pred())
    cm <- confusionMatrix(testing$Species, predictions)
    accuracy <- round(cm$overall['Accuracy'], 4)
    percent <- accuracy*100
    misclassified <- sum(testing$Species != predictions)
    total <- nrow(testing)
    paste(misclassified, "/", total)
    
  })
}