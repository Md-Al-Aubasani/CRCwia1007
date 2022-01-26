library(shiny)
library(shinythemes)
library(hpackedbubble)
library(scales)
library(reshape2)
library(highcharter)
library(DT)



deaths <- read.csv("annual-number-of-deaths-by-cause.csv")

riskfactors <- read.csv("deaths-from-cardiovascular-diseases-by-risk-factor.csv")

countries0 <- unique(deaths$Entity)

countries1 <- unique(riskfactors$Entity)

# Define UI
ui <- fluidPage(theme = shinytheme("sandstone"),
                navbarPage(tags$`color-profile`(tags$img(src = "CRC.png", height="30"), "Cardiovascular Risk Checker"),
                           tabPanel("Home",sidebarPanel(h3("Cardiovascular Risk Checker (CRC)"), tags$img(src = "CRC.png", height = "410")),mainPanel(div(includeMarkdown("Introduction.Rmd"),align = "justify"))), # Navbar 1, tabPanel
                           tabPanel("Awareness",
                                    tabsetPanel(type = "tab",
                                                tabPanel("Did you know?", tags$img(style="display: block; margin-left: auto; margin-right: auto;", src= "infograph.png", height = 2500, align = "center")),
                                                tabPanel("Interactive View",
                                                         h1("Here you can use the Interactive view to look at the causes of deaths"),
                                                         sidebarPanel(
                                                           tags$h3("Deaths depending on the cause"),
                                                           selectInput("country_g1", "Country:", c(countries0), selected = "World"),
                                                           selectInput("year_g1", "Year:", c(2000:2019)),
                                                           actionButton("button_g1", "Result"),
                                                           h6("Note: The chart of certain countries and years might not show up, as data on that was not available and hence removed.")
                                                           
                                                         ), # sidebarPanel
                                                         mainPanel(
                                                           h4(tableOutput("text_g1")),
                                                           h1(""),
                                                           hpackedbubbleOutput("plot_g1", width = "100%",height = "700")
                                                           
                                                           
                                                         )),
                                                tabPanel("Change Over Time",
                                                         sidebarPanel(
                                                           tags$h4("Change in deaths over time, by cause of death"),
                                                           selectInput("country2_g1", "Country:", c(countries0), selected = "World"),
                                                           sliderInput("year2_g1", "Period:", animate = TRUE,value = c(2000,2019), min = 2000, max = 2019),
                                                           actionButton("button2_g1", "Result"),
                                                           h6("Note: The chart of certain countries and years might not show up, as data on that was not available and hence removed.")
                                                           
                                                         ),
                                                         mainPanel(
                                                           highchartOutput("line2_g1", height = "700")
                                                         )
                                                         ),
                                                tabPanel("Simple View",
                                                         h1("Here you can use the simple view to look at the causes of death"),
                                                         sidebarPanel(
                                                           tags$h3("Deaths depending on the cause"),
                                                           selectInput("country1_g1", "Country:", c(countries0), selected = "World"),
                                                           selectInput("year1_g1", "Year:", c(2000:2019)),
                                                           actionButton("button1_g1", "Result"),
                                                           h6("Note: The chart of certain countries and years might not show up, as data on that was not available and hence removed.")
                                                           
                                                           
                                                         ), # sidebarPanel
                                                         mainPanel(
                                                           h4(tableOutput("text1_g1")),
                                                           h1(""),
                                                           plotOutput("plot1_g1",height = "500")
                                                           
                                                           
                                                         ))
                                    )),
                           tabPanel("Check Your Risk",
                                    tabsetPanel( type = "tab",
                                                 tabPanel(title = "Method 1",
                                                          sidebarPanel(
                                                            tags$h3("Framingham Risk Calculator"),
                                                            
                                                            numericInput("age_m1", "Age(30-79):", 40, min =30, max = 79),
                                                            
                                                            radioButtons("gender_m1", "Gender:", c("Male", "Female"), inline = TRUE),
                                                            
                                                            radioButtons("HTmedication_m1", "Are you on hyper tension medication?", c("Yes", "No"), inline = TRUE, selected  = "No"),
                                                            
                                                            radioButtons("smoker_m1", "Are you currently a smoker?", c("Yes", "No"), inline = TRUE, selected  = "No"),
                                                            
                                                            radioButtons("diabetes_m1", "Do you have diabetes?", c("Yes", "No"), inline = TRUE, selected  = "No"),
                                                            
                                                            tags$h6("Note: If you don't know your Systolic Blood Pressure (SBP), Total Cholestrol or HDL (High Density Lipoproteins) Cholestrol, leave them at default"),
                                                            
                                                            numericInput("sbp_m1", "Systolic Blood Pressure (default = 120 mmHg):", min =30, value = 120),
                                                            
                                                            numericInput("totChol_m1", "Total Cholestrol (default = 4.50 mmol/L):", min =2.59, max = 10, value = 4.50),
                                                            
                                                            numericInput("HDLchol_m1", "HDL Cholestrol (default = 1.5 mmol/L):", min =0.03, max = 10, value = 1.5),
                                                            
                                                            actionButton("button_m1", "Result"),
                                                            
                                                            h6("Disclaimer: This is NOT a diagnosis. This is a medically proven method of assessing someone's risk of CVD. If you want a CVD diagnosis please contact a medical professional.")
                                                            
                                                          ), # sidebarPanel
                                                          mainPanel(
                                                            h3("The calculator will give back a percentage of what your risk level might be"),
                                                            verbatimTextOutput("result_m1"),
                                                            h5("Risk is considered low if the risk score is less than 10%, moderate if it is 10% to 19%, and high if it is 20% or higher."),
                                                            h5("For more information on this method you can go through our documentation tab.")
                                                            
                                                          )),
                                                 tabPanel(title = "Method 2",
                                                          sidebarPanel(
                                                            tags$h3("WHO Risk Chart for BMI"),
                                                            
                                                            selectInput("region_m2", "Region:", c("Andean Latin America", "Australasia", "Caribbean", "Central Asia", "Central Europe", "Central Latin America", "Central Sub-Saharan Africa", "East Asia", "Eastern Europe", "Eastern Sub-Saharan Africa", "High-income Asia Pacific", "High-income North America", "North Africa and Middle East", "Oceania", "South Asia", "South-East Asia", "Southern Latin America", "Southern Sub-Saharan Africa", "Tropical Latin America", "Western Europe", "Western Sub-Saharan Africa")),
                                                            
                                                            selectInput("age_m2", "Age Group:", c("40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74")),
                                                            
                                                            radioButtons("gender_m2", "Gender:", c("Male", "Female"), inline = TRUE),
                                                            
                                                            numericInput("height_m2", "Height(in cm):", 170),
                                                            
                                                            numericInput("weight_m2", "Weight(in kg)", 70),
                                                            
                                                            radioButtons("smoker_m2", "Are you currently a smoker?", c("Yes", "No"), inline = TRUE, selected  = "No"),
                                                            
                                                            tags$h6("Note: If you don't know your Systolic Blood Pressure (SBP) leave it at default"),
                                                            
                                                            numericInput("sbp_m2", "Systolic Blood Pressure (SBP) (default = 120 mmHg):", min =30, value = 120),
                                                            
                                                            actionButton("button_m2", "Result"),
                                                            
                                                            h6("Disclaimer: This is NOT a diagnosis. This is a medically proven method of assessing someone's risk of CVD. If you want a CVD diagnosis please contact a medical professional.")
                                                            
                                                            
                                                          ), # sidebarPanel
                                                          mainPanel(
                                                            h4("Follow the instructions to get your risk level."),
                                                            tableOutput("table_m2"),
                                                            imageOutput(outputId = "result_m2", width = "200px")
                                                            
                                                          )
                                                 ),
                                                 tabPanel(title = "Method 3",
                                                          sidebarPanel(
                                                            tags$h3("WHO Risk Chart for Diabetes"),
                                                            
                                                            selectInput("region_m3", "Region:", c("Andean Latin America", "Australasia", "Caribbean", "Central Asia", "Central Europe", "Central Latin America", "Central Sub-Saharan Africa", "East Asia", "Eastern Europe", "Eastern Sub-Saharan Africa", "High-income Asia Pacific", "High-income North America", "North Africa and Middle East", "Oceania", "South Asia", "South-East Asia", "Southern Latin America", "Southern Sub-Saharan Africa", "Tropical Latin America", "Western Europe", "Western Sub-Saharan Africa")),
                                                            
                                                            selectInput("age_m3", "Age Group:", c("40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74")),
                                                            
                                                            radioButtons("gender_m3", "Gender:", c("Male", "Female"), inline = TRUE),
                                                            
                                                            radioButtons("smoker_m3", "Are you currently a smoker?", c("Yes", "No"), inline = TRUE, selected  = "No"),
                                                            
                                                            radioButtons("diabetes_m3", "Do you have diabetes?", c("Yes", "No"), inline = TRUE, selected  = "No"),
                                                            
                                                            tags$h6("Note: If you don't know your Systolic Blood Pressure (SBP) or Total Cholestrol, leave them at default"),
                                                            
                                                            numericInput("sbp_m3", "Systolic Blood Pressure (SBP) (default = 120 mmHg):", min =30, value = 120),
                                                            
                                                            numericInput("totChol_m3", "Total Cholestrol (default = 4.50 mmol/L):", min =2.59, max = 10, value = 4.50),
                                                            
                                                            actionButton("button_m3", "Result"),
                                                            
                                                            h6("Disclaimer: This is NOT a diagnosis. This is a medically proven method of assessing someone's risk of CVD. If you want a CVD diagnosis please contact a medical professional.")
                                                            
                                                            
                                                          ), # sidebarPanel
                                                          mainPanel(
                                                            h4("Follow the instructions to get your risk level."),
                                                            tableOutput("table_m3"),
                                                            imageOutput(outputId = "result_m3")
                                                            
                                                          )
                                                 )
                                    )
                           ),
                           tabPanel("Prevention",
                                    tabsetPanel(type = "tab",
                                                tabPanel("Interactive View",
                                                         h4("Here you can use the Interactive view to look at what affects cardiovascular diseases in your country or  anywhere."),
                                                         h4("You can use the insight gained to search in the prevention fact sheet,"),
                                                         sidebarPanel(
                                                           tags$h3("Deaths depending on the cause"),
                                                           selectInput("country_g2", "Country:", c(countries1), selected = "World"),
                                                           selectInput("year_g2", "Year:", c(2000:2017)),
                                                           actionButton("button_g2", "Result"),
                                                           h6("Note: The chart of certain countries and years might not show up, as data on that was not available and hence removed.")
                                                           
                                                           
                                                         ), # sidebarPanel
                                                         mainPanel(
                                                           h4(tableOutput("text_g2")),
                                                           h1(""),
                                                           hpackedbubbleOutput("plot_g2", width = "100%",height = "700")
                                                           
                                                           
                                                         )),
                                                tabPanel("Simple View",
                                                         h3("Here you can use the simple view to look at what affects cardiovascular diseases in your country or anywhere."),
                                                         h3("You can use the insight gained to search in the prevention fact sheet."),
                                                         sidebarPanel(
                                                           tags$h3("Deaths depending on the cause"),
                                                           selectInput("country1_g2", "Country:", c(countries1), selected = "World"),
                                                           selectInput("year1_g2", "Year:", c(2000:2017)),
                                                           actionButton("button1_g2", "Result"),
                                                           h6("Note: The chart of certain countries and years might not show up, as data on that was not available and hence removed.")
                                                           
                                                           
                                                         ), # sidebarPanel
                                                         mainPanel(
                                                           h4(tableOutput("text1_g2")),
                                                           h1(""),
                                                           plotOutput("plot1_g2",height = "500")
                                                           
                                                           
                                                         )),
                                                tabPanel("Prevention Fact Sheet", div(includeMarkdown("Prevention.Rmd"), align = "justify"))
                                    )),
                           tabPanel("Documentation",
                                    tabsetPanel(type = "tab", 
                                                tabPanel("Process",div(includeMarkdown("Documentation.Rmd"))),
                                                                
                                                tabPanel("1st Dataset", DT::dataTableOutput("table1")),
                                                tabPanel("2nd Dataset", DT::dataTableOutput("table2"))
                                                                )),
                           tabPanel("About Us", div(includeMarkdown("About us.Rmd"),align = "justify"))
                           
                           
                ) # navbarPage
) # fluidPage