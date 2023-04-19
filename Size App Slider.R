# Import libraries
library(shiny)
library(shinythemes)
library(data.table)
library(randomForest)

# Loading the RandomForest model
sizeModel <- readRDS("sizeModel.rds")

## UI SECTION  ###
ui <- fluidPage(theme = shinytheme("united"), #  You can also use pageWithSidebar
  
  # Page header
  headerPanel('Debbie Clothings
              Size Predictor App'),
  
  # Input values
  sidebarPanel(
    HTML("<h3>Input Measurements</h4>"),
    sliderInput("bust_C", label = "Bust Size", value = 36,
                min = min(TrainSize$bust_C),
                max = max(TrainSize$bust_C)
    ),
    sliderInput("Neck.circumference", label = "Neck Size", value = 13,
                min = min(TrainSize$Neck.circumference),
                max = max(TrainSize$Neck.circumference)),
    sliderInput("shouldercircumference", label = "Shoulder Size", value = 42,
                min = min(TrainSize$shouldercircumference),
                max = max(TrainSize$shouldercircumference)),
    sliderInput("stature", label = "Stature", value = 65,
                min = min(TrainSize$stature),
                max = max(TrainSize$stature)),
    sliderInput("Buttocks..circumference", label = "Buttocks Size", value = 40,
                min = min(TrainSize$Buttocks..circumference),
                max = max(TrainSize$Buttocks..circumference)),
    sliderInput("hip_C", label = "Hip Size", value = 35,
                min = min(TrainSize$hip_C),
                max = max(TrainSize$hip_C)),
    sliderInput("Thigh.circumference", label = "Thigh Size", value = 24,
                min = min(TrainSize$Thigh.circumference),
                max = max(TrainSize$Thigh.circumference)),
    sliderInput("waist_C", label = "Waist Size", value = 30,
                min = min(TrainSize$waist_C),
                max = max(TrainSize$waist_C)),
    
    actionButton("submitbutton", "Predict", class = "btn btn-primary")
  ),
  
  mainPanel(

    tags$label(h3('Status :')), # Status
    verbatimTextOutput('contents'),
    tags$label(h3('Prediction :')), # Output
    tableOutput('tabledata') # Prediction results table
    
  )
)


##  SERVER  ###
server<- function(input, output, session) {
  
  # Input Data
  sizeInput <- reactive({  
    
    df <- data.frame(
      Name = c("bust_C",
               "Neck.circumference",
               "shouldercircumference",
               "stature",
               "Buttocks..circumference",
               "hip_C",
               "Thigh.circumference",
               "waist_C"),
      Value = as.character(c(input$bust_C,
                             input$Neck.circumference,
                             input$shouldercircumference,
                             input$stature,
                             input$Buttocks..circumference,
                             input$hip_C,
                             input$Thigh.circumference,
                             input$waist_C)),
      stringsAsFactors = FALSE)
    
    Size <- 0
    df <- rbind(df, Size)
    input <- transpose(df)
    write.table(input,"sizeInput.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("sizeInput", ".csv", sep=""), header = TRUE)
    
    Output <- data.frame(Prediction=predict(sizeModel,test), round(predict(sizeModel,test,type="prob"), 3))
    print(Output)
    
  })
  
  # Status
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Prediction is Complete.") 
    } else {
      return("Server is ready for Prediction.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(sizeInput()) 
    } 
  })
  
}

##  SHINY APP FUNCTION  ##

shinyApp(ui = ui, server = server)

