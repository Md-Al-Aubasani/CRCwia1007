
library(hpackedbubble)
library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(reshape2)
library(highcharter)
library(DT)


deaths <- read.csv("annual-number-of-deaths-by-cause.csv")

riskfactors <- read.csv("deaths-from-cardiovascular-diseases-by-risk-factor.csv")

causes = c("Execution", "Meningitis", "Cancers", "Fire", "Malaria", "Drowning", "Homicide", "HIV/AIDS", "Drug use disorders", "Tuberculosis", "Road injuries", "Maternal disorders", "Lower respiratory infections", "Neonatal disorders", "Alcohol use disorders", "Natural disasters", "Diarrheal diseases", "Heat (hot and cold exposure)", "Nutritional deficiencies", "Suicide", "Conflict", "Diabetes", "Poisonings", "Protein-energy malnutrition", "Terrorism", "Cardiovascular diseases", "Kidney disease", "Respiratory diseases", "Liver diseases", "Digestive diseases", "Hepatitis", "Dementia", "Parkinson's disease")

factors = c("Diet high in processed meat", "Diet high in sugar-sweetened beverages", "Diet low in fiber", "Diet low in legumes", "Diet low in polyunsaturated fatty acids", "Diet low in vegetables", "Lead exposure", "Secondhand smoke", "Diet high in sodium", "Diet high in trans fatty acids", "Diet low in fruits", "Diet low in nuts and seeds", "Diet low in seafood omega-3 fatty acids", "Diet low in whole grains", "Particulate matter pollution", "Smoking")

server <- function(input, output) {
  #this is to make the first packed bubble graph
  bubble_graph <- reactive({
    country <- deaths[deaths$Entity==input$country_g1,]
    year <- country[country$Year == input$year_g1,]
    selection<-round(as.numeric(unlist(year[,4:36], use.names = FALSE)))
    
    
    if (input$country_g1=="World"){tlt = paste("CAUSES OF DEATHS IN THE WORLD"," (",input$year_g1 ,")", sep = "")}
    else {tlt = paste("CAUSES OF DEATHS IN ", toupper(input$country_g1)," (",input$year_g1 ,")", sep = "")}
    
    a <- hpackedbubble( value = selection,name = causes,cat =causes,
                        title = tlt,
                        pointFormat = "{point.y} Deaths",
                        dataLabelsFilter = min(selection)+1000,
                        packedbubbleMinSize = "25%",
                        packedbubbleMaxSize = "220%",
                        theme = "gridlight",
                        packedbubbleZMin = min(selection),
                        packedbubbleZmax = max(selection), split = 0,
                        gravitational = 0.02,
                        parentNodeLimit = 0,
                        dragBetweenSeries = 0,
                        width = "100%",
                        height = "700")
    return(a)
  })
  
  #this is to make the first simple graph
  simple_graph <- reactive({
    country <- deaths[deaths$Entity==input$country1_g1,]
    year <- country[country$Year == input$year1_g1,]
    selection<-round(as.numeric(unlist(year[,4:36], use.names = FALSE)))
    
    
    if (input$country1_g1=="World"){tlt = paste("CAUSES OF DEATHS IN THE WORLD"," (",input$year1_g1 ,")", sep = "")}
    else {tlt = paste("CAUSES OF DEATHS IN ", toupper(input$country1_g1)," (",input$year1_g1 ,")", sep = "")}
    
    df <- data.frame(causes, selection)
    df1 <- df[!(is.na(df$selection)),]
    
    a <- ggplot(data = df1,aes(x=reorder(causes,selection),y=selection)) + 
      geom_bar(stat ='identity', aes(fill = selection))+
      scale_fill_gradient(name="Deaths",low = muted("blue"),
                          high = muted("red", l= 50, c= 150))+
      theme_bw()+
      scale_y_continuous(labels = scales::comma, breaks = breaks_pretty(7))+
      coord_flip()+
      labs(title = tlt,y='Deaths',x='Causes')
    
    
    return(a)
  })
  
  #this calculates risk based on the formula
  riskCalc <- reactive({
    
    if (input$gender_m1 == "Male"){
      AgeFactor = 3.06117
      TotalCholFactor = 1.12370
      HDLCholFactor = -0.93263
      AvgRisk = 23.9802
      RiskPeriodFactor = 0.88936
      
      if (input$HTmedication_m1 == "Yes"){SysBPFactor = 1.99881}
      else{SysBPFactor = 1.93303}
      
      if (input$diabetes_m1 == "Yes"){DM = 0.57367}
      else {DM = 0}
      
      if (input$smoker_m1 == "Yes"){Cig = 0.65451}
      else{Cig = 0}
      
    }else{
      AgeFactor = 2.32888
      TotalCholFactor = 1.20904
      HDLCholFactor = -0.70833
      AvgRisk = 26.1931
      RiskPeriodFactor = 0.95012
      
      if (input$HTmedication_m1 == "Yes"){SysBPFactor = 2.82263}
      else{SysBPFactor = 2.76157}
      
      if (input$diabetes_m1 == "Yes"){DM = 0.69154}
      else {DM = 0}
      
      if (input$smoker_m1 == "Yes"){Cig = 0.52873}
      else{Cig = 0}
      
    }
    
    
    
    riskfactors = (log(input$age_m1) * AgeFactor) + (log(input$totChol_m1*38.67) * TotalCholFactor) + (log(input$HDLchol_m1*38.67) * HDLCholFactor) + (log(input$sbp_m1) * SysBPFactor) + Cig + DM - AvgRisk
    
    riskexp = exp(riskfactors)
    
    risk = 100*(1 - RiskPeriodFactor^riskexp)
    
    risk <- paste0("Your cardiovascular risk percentage is ",round(risk, digits = 2),"%")
    
    return(risk)
    
  })
  
  #this is to display the top 3 causes of death for bubble graph
  bubble_text<- reactive({
    country <- deaths[deaths$Entity==input$country_g1,]
    year <- country[country$Year == input$year_g1,]
    selection<-round(as.numeric(unlist(year[,4:36], use.names = FALSE)))
    
    df <- data.frame(causes, selection)
    s<- arrange(df, desc(selection))
    a<- paste0("The largest cause of death was ", s[1,1], ", with ", format(s[1,2], big.mark = ",", scientific = FALSE, trim = TRUE), " deaths.")
    b<- paste0("The second largest cause of death was ", s[2,1], " with ", format(s[2,2], big.mark = ",", scientific = FALSE, trim = TRUE), " deaths.")
    c<- paste0("The third largest cause of death was ", s[3,1], ", with ", format(s[3,2], big.mark = ",", scientific = FALSE, trim = TRUE), " deaths.")
    
    `Top-3-causes-of-deaths` <- c(a, b, c)
    df<- data.frame(`Top-3-causes-of-deaths`)
    
    return(df)
  })
  
  #this is to display the top 3 causes of death for the first simple graph
  simple_text <-reactive({
    country <- deaths[deaths$Entity==input$country1_g1,]
    year <- country[country$Year == input$year1_g1,]
    selection<-round(as.numeric(unlist(year[,4:36], use.names = FALSE)))
    
    df <- data.frame(causes, selection)
    s<- arrange(df, desc(selection))
    a<- paste0("The largest cause of death was ", s[1,1], ", with ", format(s[1,2], big.mark = ",", scientific = FALSE, trim = TRUE), " deaths.")
    b<- paste0("The second largest cause of death was ", s[2,1], " with ", format(s[2,2], big.mark = ",", scientific = FALSE, trim = TRUE), " deaths.")
    c<- paste0("The third largest cause of death was ", s[3,1], ", with ", format(s[3,2], big.mark = ",", scientific = FALSE, trim = TRUE), " deaths.")
    
    `Top-3-causes-of-deaths` <- c(a, b, c)
    df<- data.frame(`Top-3-causes-of-deaths`)
    
    return(df)
  })
  
  #this is to display the top 3 risk factors for bubble graph
  bubble2_text<- reactive({
    country <- riskfactors[riskfactors$Entity==input$country_g2,]
    year <- country[country$Year == input$year_g2,]
    selection<-round(as.numeric(unlist(year[,4:19], use.names = FALSE)))
    
    df <- data.frame(factors, selection)
    s<- arrange(df, desc(selection))
    a<- paste0("The largest cause of death was ", s[1,1], ", with ", format(s[1,2], big.mark = ",", scientific = FALSE, trim = TRUE), " deaths.")
    b<- paste0("The second largest cause of death was ", s[2,1], " with ", format(s[2,2], big.mark = ",", scientific = FALSE, trim = TRUE), " deaths.")
    c<- paste0("The third largest cause of death was ", s[3,1], ", with ", format(s[3,2], big.mark = ",", scientific = FALSE, trim = TRUE), " deaths.")
    
    `Top-3-risk-factors` <- c(a, b, c)
    df<- data.frame(`Top-3-risk-factors`)
    
    return(df)
  })
  
  #this is to display the top 3 risk factors for simple graph
  simple2_text <-reactive({
    country <- riskfactors[riskfactors$Entity==input$country1_g2,]
    year <- country[country$Year == input$year1_g2,]
    selection<-round(as.numeric(unlist(year[,4:19], use.names = FALSE)))
    
    df <- data.frame(factors, selection)
    s<- arrange(df, desc(selection))
    a<- paste0("The largest cause of death was ", s[1,1], ", with ", format(s[1,2], big.mark = ",", scientific = FALSE, trim = TRUE), " deaths.")
    b<- paste0("The second largest cause of death was ", s[2,1], " with ", format(s[2,2], big.mark = ",", scientific = FALSE, trim = TRUE), " deaths.")
    c<- paste0("The third largest cause of death was ", s[3,1], ", with ", format(s[3,2], big.mark = ",", scientific = FALSE, trim = TRUE), " deaths.")
    
    `Top-3-risk-factors` <- c(a, b, c)
    df<- data.frame(`Top-3-risk-factors`)
    
    return(df)
  })
  
  
  #this is to make the second packed bubble graph
  bubble2_graph <- reactive({
    country <- riskfactors[riskfactors$Entity==input$country_g2,]
    year <- country[country$Year == input$year_g2,]
    selection<-round(as.numeric(unlist(year[,4:19], use.names = FALSE)))
    
    
    if (input$country_g2=="World"){tlt = paste("DEATH FROM CARDIOVASCULAR DISEASES, BY RISK FACTOR, IN THE WORLD"," (",input$year_g2 ,")", sep = "")}
    else {tlt = paste("DEATH FROM CARDIOVASCULAR DISEASES, BY RISK FACTOR, IN ", toupper(input$country_g2)," (",input$year_g2 ,")", sep = "")}
    
    a <- hpackedbubble( value = selection,name = factors,cat =factors,
                        title = tlt,
                        pointFormat = "{point.y} Deaths",
                        dataLabelsFilter = min(selection)+700,
                        packedbubbleMinSize = "25%",
                        packedbubbleMaxSize = "170%",
                        theme = "gridlight",
                        packedbubbleZMin = min(selection),
                        packedbubbleZmax = max(selection), split = 0,
                        gravitational = 0.02,
                        parentNodeLimit = 0,
                        dragBetweenSeries = 0,
                        width = "100%",
                        height = "700")
    return(a)
  })
  
  #this is to make the second simple graph
  simple2_graph <- reactive({
    country <- riskfactors[riskfactors$Entity==input$country1_g2,]
    year <- country[country$Year == input$year1_g2,]
    selection<-round(as.numeric(unlist(year[,4:19], use.names = FALSE)))
    
    
    if (input$country1_g2=="World"){tlt = paste("DEATH FROM CARDIOVASCULAR DISEASES, BY RISK FACTOR, IN THE WORLD"," (",input$year1_g2 ,")", sep = "")}
    else {tlt = paste("DEATH FROM CARDIOVASCULAR DISEASES, BY RISK FACTOR, IN ", toupper(input$country1_g2)," (",input$year1_g2 ,")", sep = "")}
    
    df <- data.frame(factors, selection)
    df1 <- na.omit(df)
    
    
    a <- ggplot(data = df1,aes(x=reorder(factors,selection),y=selection)) + 
      geom_bar(stat ='identity', aes(fill = selection))+
      theme_bw() +
      scale_fill_gradient(name="Deaths",low = muted("blue"),
                          high = muted("red", l= 50, c= 150))+
      scale_y_continuous(labels = scales::comma, breaks = breaks_pretty(7))+
      coord_flip()+
      labs(title = tlt,y='Deaths',x='Risk Factors')
    
    return(a)
  })
  
  
  
  #
  #these are all outputs
  #
  output$result_m1 <- renderText({
    if (input$button_m1>0){isolate(riskCalc())}
  })
  
  output$plot_g1 <- renderHpackedbubble({
    if (input$button_g1>0 ){
      isolate(bubble_graph())
    }
  })
  
  output$plot1_g1 <- renderPlot({
    if (input$button1_g1>0){
      isolate(simple_graph())
    }
  })
  
  output$text_g1 <- renderTable({
    if (input$button_g1>0){
      isolate(bubble_text())
    }
  }, colnames = FALSE)
  output$text1_g1 <- renderTable({
    if (input$button1_g1>0){
      isolate(simple_text())
    }
  }, colnames = FALSE)
  
  output$plot_g2 <- renderHpackedbubble({
    if (input$button_g2>0 ){
      isolate(bubble2_graph())
    }
  })
  
  output$plot1_g2 <- renderPlot({
    if (input$button1_g2>0){
      isolate(simple2_graph())
    }
  })
  
  output$text_g2 <- renderTable({
    if (input$button_g2>0){
      isolate(bubble2_text())
    }
  }, colnames = FALSE)
  output$text1_g2 <- renderTable({
    if (input$button1_g2>0){
      isolate(simple2_text())
    }
  }, colnames = FALSE)
  
  output$line2_g1 <- renderHighchart({
    if(input$button2_g1>0){
      isolate(graph3())
    }
  })
  
  output$table1 = DT::renderDataTable({
    colnames(deaths) <- c("Entity", "Code", "Year", causes)
    deaths
  })
  
  output$table2 = DT::renderDataTable({
    colnames(riskfactors) <- c("Entity", "Code", "Year", factors)
    riskfactors
  })
  
  graph3 <- reactive({
    colnames(deaths) <- c("Entity", "Code", "Year", causes)
    
    country <- deaths[deaths$Entity == input$country2_g1,]
    
    year <- filter(country, country$Year>=input$year2_g1[1] & country$Year<=input$year2_g1[2])
    
    df <- select(year, -Code, -Entity)
    
    df <- melt(df, id.vars = "Year")
    
    df$value <- round(as.numeric(unlist(df$value, use.names = FALSE)))
    
    df<-arrange(df, variable,  Year)
    
    hc <- df %>%
      hchart('line', hcaes(x = Year, y = value, group = variable))
    
    return(hc)
  })
  
  
  
  #this is to display the chart and instructions for method 2
  observe({
    
    STEPS <- c("1.", "2.", "3.", "4.", "5.", "6.")
    INSTRUCTIONS <- c("", "", "", "", "", "")
    bmi <- input$weight_m2/((input$height_m2/100)^2)
    
    rbmi <- round(bmi, digits = 1)
    
    INSTRUCTIONS[1] = paste("Look for the section for ",input$gender_m2, "s (near the top)", sep = "")
    if (input$smoker_m2=="Yes"){INSTRUCTIONS[2] = "Under it look for the smoker section"}
    else {INSTRUCTIONS[2] = "Under it look for the non-smoker section"}
    INSTRUCTIONS[3] = paste("Go into the age group of ", input$age_m2, sep ="")
    INSTRUCTIONS[5] = paste("Your BMI is ", rbmi, ". Look for the COLUMN with the appropriate BMI (near the bottom)", sep = "")
    INSTRUCTIONS[4] = paste("Your SBP (Systolic Blood Pressure) is ", input$sbp_m2, " mmHg. Look for the ROW with the appropriate SBP (right side)", sep = "")
    INSTRUCTIONS[6] = "Where these two intersect is your risk level. The color of the box determines your risk"
    
    df<- data.frame(STEPS, INSTRUCTIONS)
    
    
    
    if (input$button_m2>0){
      
      output$table_m2 <- renderTable(df)
      output$result_m2 <-renderImage({
        
        isolate(l<- paste0("www/BMI/", input$region_m2, " BMI.png", sep=""))
        isolate(return(list(src = l, height = "1000")))
        
        
      },deleteFile=FALSE)}})
  
  
  #this is to display the chart and instructions for method 3
  observe({
    
    STEPS <- c("1.", "2.", "3.", "4.", "5.", "6.", "7.")
    INSTRUCTIONS <- c("", "", "", "", "", "", "")
    
    if (input$diabetes_m3 == "Yes"){INSTRUCTIONS[1] = "Look for the section for people with diabetes"}
    else{INSTRUCTIONS[1] = "Look for the section for people without diabetes"}
    INSTRUCTIONS[2] = paste("Look for the section for ",input$gender_m2, "s (near the top)", sep = "")
    if (input$smoker_m2=="Yes"){INSTRUCTIONS[3] = "Under it look for the smoker section"}
    else {INSTRUCTIONS[3] = "Under it look for the non-smoker section"}
    INSTRUCTIONS[4] = paste("Go into the age group of ", input$age_m2, sep ="")
    INSTRUCTIONS[5] = paste("Your SBP (Systolic Blood Pressure) is ", input$sbp_m2, " mmHg. Look for the ROW with the appropriate SBP (right side)", sep ="")
    INSTRUCTIONS[6] = paste("Your total cholestrol is ", input$totChol_m3, " mmol/L. Look for the COLUMN with the appropriate value (near the bottom)", sep = "")
    INSTRUCTIONS[7] = "Where these two intersect is your risk level. The color of the box determines your risk"
    
    df<- data.frame(STEPS, INSTRUCTIONS)
    
    if (input$button_m3>0){
      output$table_m3 <- renderTable(df)
      output$result_m3 <-renderImage({
        
        isolate(l<- paste0("www/DB/", input$region_m3, " DB.png", sep=""))
        isolate(return(list(src = l, height = "800")))
        
        
      },deleteFile=FALSE)
    }
  })
} # server
