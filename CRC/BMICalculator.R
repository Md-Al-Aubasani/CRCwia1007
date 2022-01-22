
## BMI Calculator [Work-in-progress]

library(shiny)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("superhero"),
                navbarPage("BMI Calculator:",
                           
                           tabPanel("Body Mass Index",
                                    # Input values
                                    sidebarPanel(
                                      HTML("<h3>Input your Height & Weight</h3>"),
                                      numericInput("height", 
                                                  label = "Height", 
                                                  value = 175, 
                                                  min = 70, 
                                                  max = 250),
                                      numericInput("weight", 
                                                  label = "Weight", 
                                                  value = 70, 
                                                  min = 35, 
                                                  max = 120),
                                      
                                      actionButton("submitbutton", 
                                                   "Calculate", 
                                                   class = "btn btn-primary")
                                    ),
                                    
                                    mainPanel(
                                      tags$label(h3('Calculate Your Body Mass Index')), # Status/Output Text Box
                                      verbatimTextOutput('contents'),
                                      tableOutput('tabledata') # Results table
                                    ) # mainPanel()
                                    
                           )
                ) # navbarPage()
) # fluidPage()

## Server
server <- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    bmi <- input$weight/( (input$height/100) * (input$height/100) )
    bmi <- data.frame(bmi)
    names(bmi) <- "BMI"
    print(bmi)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Please press the 'Calculate' button to check BMI.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
