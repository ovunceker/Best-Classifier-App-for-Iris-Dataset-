library(shiny)
library(plotly)

fluidPage(
  titlePanel("Find the Best Classifier"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        inputId = "choice",
        label = "Select one option:",
        choices = c("Linear SVM", "Radial SVM", "Decision Tree",
                    "Random Forest", "LDA", "QDA")
      ),
      conditionalPanel(
        condition = "input.choice == 'Linear SVM'",
        selectInput("sliderSVMlin", "Select a Cost parameter:",
                    choices = c(0.01, 0.1, 1, 10),
                    selected = 0.1)
      ),
      conditionalPanel(
        condition = "input.choice == 'Radial SVM'",
        selectInput("sliderSVMrad", "Select a Cost parameter:",
                    choices = c(0.01, 0.1, 1, 10),
                    selected = 0.1)
      ),
      conditionalPanel(
        condition = "input.choice == 'Decision Tree'",
        selectInput("sliderTree", "Select a Complexity parameter:",
                    choices = c(0.001, 0.01, 0.1, 0.5),
                    selected = 0.01)
      ),
      conditionalPanel(
        condition = "input.choice == 'Random Forest'",
        selectInput("sliderForest1", "Select the Number of Randomly Selected Features:",
                    choices = c(1,2),
                    selected = 1)
      ),
      conditionalPanel(
        condition = "input.choice == 'Random Forest'",
        selectInput("sliderForest2", "Select the Number of Trees:",
                    choices = c(100, 500, 1000),
                    selected = 500)
      ),
      actionButton("submit", "Submit")
    ),
    
    mainPanel(
      h5("Choose classifier and change parameters and click submit!"),
      plotlyOutput("plot"),
      h3("Current model accuracy is:"),
      textOutput("predictions"),
      h3("Number of missclassified items / total items:"),
      textOutput("missclassified")
      
    )
  )
)