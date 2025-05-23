
# getwd() # Adjust as needed to "inseason-escapement" directory

library(tidyverse)
library(scales)
library(shiny)
library(shinythemes)
library(waiter)
library(DT)
library(gridExtra)
library(rsconnect)
library(ggrepel)

rm(list = ls())

CurrentYear <- 2024 ## Re-name to current in-season year 

theme_set(theme_bw(base_size = 20)+
            theme(panel.grid.major.y = element_line(color = "gray80",linewidth = 0.5,linetype = 2), 
                  panel.grid.minor.y = element_line(color = "gray80",linewidth = 0.5,linetype = 2),
                  panel.grid.major.x = element_blank(), 
                  panel.grid.minor.x = element_blank(),
                  panel.border = element_blank(),
                  axis.line = element_line(color = "black"),
                  axis.text = element_text(color = "black"),
                  axis.title = element_text(color = "black", size = 20, face = "bold"),
                  axis.text.x = element_text(angle = 270,vjust=-0.1)))

load("data/HistoricRTC.RData")
load(paste0("data/Inseason_",CurrentYear,"_AUC.RData"))

# User Interface ----------------------------------------------------------------------

ui <- fluidPage(
  useWaiter(), 
  waiterPreloader(), #optional loading screen that only shows briefly
  theme = shinytheme("sandstone"),
  navbarPage(paste(CurrentYear,"In-Season Run Timing Curve Visualization"), 
             tabPanel("Charts",
                      sidebarLayout(
                        sidebarPanel(
                          width = 2,
                          h2("Chart Options"),
                          conditionalPanel(condition = "input.mainpanels_id == 'District-Wide Aggregate SEG Goals'",
                                           selectInput(inputId = "speciesInput1",
                                                       label = h3("Species"),
                                                       choices = list("Pink Salmon" = "Pink Salmon",
                                                                      "Chum Salmon" = "Chum Salmon")),
                                           radioButtons(inputId = "oddevenInput1", 
                                                       label = h3("Odd/Even Year Run-Timing"), 
                                                       choices = c("")),
                                           selectInput(inputId = "districtInput1",
                                                       label = h3("District"),
                                                       c("")),
                                           selectInput(inputId = "yearInput1",
                                                       label = h3("Historical RTC Year Range"),
                                                       c("")),
                                           selectInput(inputId = "example.yearInput1",
                                                       label = h3("Plot historical year"),
                                                       choices = c(""),
                                                       multiple = TRUE),
                                           checkboxGroupInput(inputId = "surveyType1", 
                                                              label = h3("Survey Method"),
                                                              choices = list("Aerial" = "Aerial",  
                                                                             "Ground" = "Ground"), 
                                                              selected = c("Aerial","Ground")),
                                           checkboxGroupInput(inputId = "surveyCondition1", 
                                                              label = h3("Survey Grade"),
                                                              choices = list(
                                                                "1" = 1,
                                                                "2" = 2,
                                                                "3" = 3,
                                                                "4" = 4,
                                                                "5" = 5,
                                                                "NA" = NA
                                                              ),
                                                              selected = c("1","2","3","4","5","NA"))),
                          conditionalPanel(condition = "input.mainpanels_id == 'Individual-Stock Management Objective Goals'",
                                           selectInput(inputId = "speciesInput2",
                                                       label = h3("Species"),
                                                       choices = list("Pink Salmon" = "Pink Salmon",
                                                                      "Chum Salmon" = "Chum Salmon",
                                                                      "Sockeye Salmon" = "Sockeye Salmon")),
                                           radioButtons(inputId = "oddevenInput2", 
                                                        label = h3("Odd/Even Year Run-Timing"), 
                                                        choices = c("")),
                                           selectInput(inputId = "stockInput2",
                                                       label = h3("Stock"),
                                                       c("")),
                                           selectInput(inputId = "yearInput2",
                                                       label = h3("Historical RTC Year Range"),
                                                       c("")),
                                           selectInput(inputId = "example.yearInput2",
                                                       label = h3("Plot historical year"),
                                                       choices = c(""),
                                                       multiple = TRUE),
                                           checkboxGroupInput(inputId = "surveyType2", 
                                                              label = h3("Survey Method"),
                                                              choices = list("Aerial" = "Aerial",  
                                                                             "Ground" = "Ground"), 
                                                              selected = c("Aerial","Ground")),
                                           checkboxGroupInput(inputId = "surveyCondition2", 
                                                              label = h3("Survey Grade"),
                                                              choices = list(
                                                                "1" = 1,
                                                                "2" = 2,
                                                                "3" = 3,
                                                                "4" = 4,
                                                                "5" = 5,
                                                                "NA" = NA
                                                              ),
                                                              selected = c("1","2","3","4","5","NA")))),
                          mainPanel(
                          width = 10,
                          tabsetPanel(
                            tabPanel(title = "District-Wide Aggregate SEG Goals", 
                                     plotOutput("agg.seg.escape.1", height = "350px", hover = "plot_hover1"),
                                     verbatimTextOutput("plot_info1"),
                                     plotOutput("agg.seg.escape.2", height = "350px", hover = "plot_hover2"),
                                     verbatimTextOutput("plot_info2"),
                                     DTOutput("agg.seg.escape.table")),
                            tabPanel(title = "Individual-Stock Management Objective Goals", 
                                     plotOutput("manage.obj.escape.1", height = "350px", hover = "plot_hover3"),
                                     verbatimTextOutput("plot_info3"),
                                     plotOutput("manage.obj.escape.2", height = "350px", hover = "plot_hover4"),
                                     verbatimTextOutput("plot_info4"),
                                     DTOutput("manage.obj.escape.table")),
                            id="mainpanels_id"
                          )
                        )
                      )
             ),
             tabPanel("About", 
                      h3("About this application:"),
                      h4("This application is designed to take in-season escapement data and compare them to historical
                        run-timing curves to assess how a given stock's escapement is progressing within the current year.
                        A variety of methods were employed to estimate escapement across different streams and stocks: aerial 
                        surveys, ground (stream walking) surveys, and weir (physical or video) counts."),
                      h4("Aerial and ground survey counts are used to calculate a final escapement index for each stream based on 
                         the area-under-the-curve method (pink and chum salmon) or the peak survey count (sockeye salmon)."),
                      h4("Historical run-timing curves are calculated using a glm binomial model. The percent of escapement is modeled
                        as a function of the Julian day-of-year. Three run-timing models are created. The first model includes 
                        all escapement data collected from 1976 to the year prior to the current year, the second includes only the
                        previous 10 years of escapement data, and the third includes the previous 20 years of escapement data."),
                      h4("Current year escapement is compared to differing types of escapement goals: District-wide aggregate 
                      sustainbable escapement goals (SEG's) and individual-stock management objectives."),
                      br(),
                      p("If you have questions or suggestions, please contact Jonah Bacon,
                             jonah.bacon@alaska.gov"),
                      br(),
                      p("Application version 1.1")
             )
             
  ))


# Server ------------------------------------------------------------------


server <- function(input, output, session) {
  
  ## Dynamic drop-down lists - SEG's ----
  
  observeEvent(input$speciesInput1,
               {
                 updateRadioButtons(session,
                                   inputId = "oddevenInput1",
                                   choices = unique(RTC.AGG.df[(RTC.AGG.df$Species == input$speciesInput1), ]$PinkOddEven)
                 )
               }
  )
  observeEvent(input$speciesInput1,
               {
                 updateSelectInput(session,
                                   inputId = "districtInput1",
                                   choices = unique(RTC.AGG.df[(RTC.AGG.df$Species == input$speciesInput1), ]$District)
                 )
               }
  )
  observeEvent(input$districtInput1,
               {
                 # b <- input$districtInput1
                 updateSelectInput(session, 
                                   inputId = "yearInput1",
                                   choices = unique(RTC.AGG.df[(RTC.AGG.df$Species == input$speciesInput1 & RTC.AGG.df$District == input$districtInput1), ]$YEAR.RANGE)
                 )
               }
  )
  observeEvent(input$districtInput1,
               {
                 updateSelectInput(session, 
                                   inputId = "example.yearInput1",
                                   choices = unique(HIST.AY.RT.df[(HIST.AY.RT.df$SPECIES == input$speciesInput1 & HIST.AY.RT.df$District == input$districtInput1), ]$Year)
                 )
               }
               )

  ## Make data reactive - SEG's ----
  
  example.year.agg_data <- reactive({
    
    HIST.AY.RT.df %>% filter(SPECIES == input$speciesInput1 & District == input$districtInput1 & Year %in% c(input$example.yearInput1))
    
  })
  
  aggregate_data <- reactive({
    
    RTC.AGG.df %>% filter(Species == input$speciesInput1 & PinkOddEven == input$oddevenInput1 & District == input$districtInput1 & YEAR.RANGE == input$yearInput1)
    
  })
  
  aggregate.CY_data <- reactive({
    
    AGG.CY.AUC.df %>%
      filter(Species == input$speciesInput1 & PinkOddEven == input$oddevenInput1 & District == input$districtInput1 & YEAR.RANGE == input$yearInput1)
    
  })
  
  aggregate.CY.surv_data <- reactive({
    
    AGG.CY.AUC.surv.df %>% 
      filter(Species == input$speciesInput1 & District == input$districtInput1 & SurveyType %in% c(input$surveyType1) & Stream_Condition %in% c(input$surveyCondition1))
    
  })
  
  aggregate.CY_table <- reactive({
    
    AGG.CY.AUC.table %>% 
      filter(Species == input$speciesInput1 & PinkOddEven == input$oddevenInput1 & District == input$districtInput1 & YEAR.RANGE == input$yearInput1 & SurveyType %in% c(input$surveyType1) & Stream_Condition %in% c(input$surveyCondition1)) %>% 
      ungroup() %>% 
      select(-c(Species,District,PinkOddEven,YEAR.RANGE))
    
  })
  
  
  ## Output plots - SEG's ----
  
  output$agg.seg.escape.1 <- renderPlot({
    
    ggplot() +
      
      geom_line(data = aggregate_data(), aes(x = OriginDate, y = LowerSEG), color ="steelblue", linewidth = 0.7) +
      geom_ribbon(data = aggregate_data(), aes(x = OriginDate, ymin = LowerSEG_se_lwr, ymax = LowerSEG_se_upr), fill = "steelblue", alpha = 0.3) +
      
      geom_line(data = aggregate_data(), aes(x = OriginDate, y = UpperSEG), color ="steelblue", linewidth = 0.7) +
      geom_ribbon(data = aggregate_data(), aes(x = OriginDate, ymin = UpperSEG_se_lwr, ymax = UpperSEG_se_upr), fill = "steelblue", alpha = 0.3) +
      
      geom_ribbon(data = aggregate_data(), aes(x = OriginDate, ymin = LowerSEG_se_upr, ymax = UpperSEG_se_lwr), fill = "steelblue", alpha = 0.1) +
      
      geom_line(data = aggregate.CY_data(), aes(x = as.Date(JulianDay, origin = as.Date("0000-01-01")), y = DistSumEscape),
                color = "red", lty = 2, linewidth = 1, alpha = 0.5) +
      geom_point(data = aggregate.CY_data(), aes(x = as.Date(JulianDay, origin = as.Date("0000-01-01")), y = DistSumEscape),
                 shape = 25, fill = "red", color = "black", size = 3) +
      # geom_line(data = example.year.agg_data(), aes(x = as.Date(JulianDay, origin = as.Date("0000-01-01")), y = DistSumEscape),
      #           color = "blue", lty = 3, linewidth = 1, alpha = 0.5) +
      # geom_point(data = example.year.agg_data(), aes(x = as.Date(JulianDay, origin = as.Date("0000-01-01")), y = DistSumEscape),
      #            shape = 21, fill = "blue", color = "black", size = 3) +
      

      scale_x_date(breaks = "1 week", date_labels = "%b %d", limits = c(as.Date(121, origin = as.Date("0000-01-01")),as.Date(296, origin = as.Date("0000-01-01"))), expand = c(0.02,0.02)) +
      xlab("Date") +
      ylab("Cumulative Escapement")
    
    # print(p1)
    
  })
  
  output$agg.seg.escape.2 <- renderPlot({
    
    ggplot(data = aggregate.CY.surv_data(), aes(x = as.Date(JulianDay, origin = as.Date("0000-01-01")), y=SurveyCount, 
                                                label = Stream_Condition, color = Stock, fill = Stock, shape = SurveyType)) + ## AUC plot
      
      geom_line(linewidth = 1, alpha = 0.5) +
      geom_point(color = "black", size = 3, show.legend = FALSE) +
      scale_shape_manual(values = c(21,22,24)) + 

      scale_x_date(breaks = "1 week", date_labels = "%b %d", limits = c(as.Date(121, origin = as.Date("0000-01-01")),as.Date(296, origin = as.Date("0000-01-01"))), expand = c(0.02,0.02)) +
      xlab("Date") +
      ylab("Survey Count") +

      theme(legend.position = "inside",
            legend.margin = margin(0, 0, 0, 0), # turned off for alignment
            legend.justification.inside = c(0.02,1),
            legend.box.background = element_rect(color = "black", linewidth = 1, linetype = 1),
            legend.box.margin = margin(5, 5, 5, 5))
    
    # print(p2)

  })
  
  output$agg.seg.escape.table <- renderDataTable(aggregate.CY_table())
  
  ## Dynamic drop-down lists - ISMO's ----
  
  # observeEvent(input$speciesInput2,
  #              {
  #                updateSelectInput(session,
  #                                  inputId = "oddevenInput2",
  #                                  choices = unique(RTC.IND.df[(RTC.IND.df$Species == input$speciesInput2), ]$PinkOddEven)
  #                )
  #              }
  # )
  observeEvent(input$speciesInput2,
               {
                 updateRadioButtons(session,
                                    inputId = "oddevenInput2",
                                    choices = unique(RTC.IND.df[(RTC.IND.df$Species == input$speciesInput2), ]$PinkOddEven)
                 )
               }
  )
  # observeEvent(input$speciesInput1,
  #              {
  #                updateRadioButtons(session,
  #                                   inputId = "oddevenInput1",
  #                                   choices = unique(RTC.AGG.df[(RTC.AGG.df$Species == input$speciesInput1), ]$PinkOddEven)
  #                )
  #              }
  # )
  observeEvent(input$speciesInput2,
               {
                 x <- input$speciesInput2
                 updateSelectInput(session, 
                                   inputId = "stockInput2",
                                   choices = unique(RTC.IND.df[(RTC.IND.df$Species == input$speciesInput2 & RTC.IND.df$TYPE.STOCK == "Individual"), ]$Stock)
                 )
               }
  )
  observeEvent(input$stockInput2,
               {
                 y <- input$stockInput2
                 updateSelectInput(session, "yearInput2",
                                   choices = unique(RTC.IND.df[(RTC.IND.df$Species == input$speciesInput2 & RTC.IND.df$Stock == input$stockInput2), ]$YEAR.RANGE)
                 )
               }
  )
  observeEvent(input$stockInput2,
               {
                 updateSelectInput(session, "example.yearInput2",
                                   choices = unique(HIST.AY.RT.df[(HIST.AY.RT.df$SPECIES == input$speciesInput2 & HIST.AY.RT.df$STOCK == input$stockInput2), ]$Year)
                 )
               }
  )

  ## Make data reactive - ISMO's ----
  
  example.year.ind.stock_data <- reactive({
    
    HIST.AY.RT.df %>% filter(SPECIES == input$speciesInput2 & STOCK == input$stockInput2 & Year %in% c(input$example.yearInput2))
    
  })
  
  ind.stock_data <- reactive({
    
      RTC.IND.df %>% filter(Species == input$speciesInput2 & PinkOddEven == input$oddevenInput2 & Stock == input$stockInput2 & YEAR.RANGE == input$yearInput2)

  })
  
  ind.stock.CY_data <- reactive({
    
    IND.CY.AUC.df %>% 
      filter(Species == input$speciesInput2 & Stock == input$stockInput2 & SurveyType %in% c(input$surveyType2) & Stream_Condition %in% c(input$surveyCondition2))
    
  })
  
  ind.stock.CY_table <- reactive({
    
    IND.CY.AUC.table %>% 
      filter(Species == input$speciesInput2 & Stock == input$stockInput2 & SurveyType %in% c(input$surveyType2) & Stream_Condition %in% c(input$surveyCondition2)) %>% 
      ungroup() %>% 
      select(-c(Species,Stock))
    
  })
  
  ## Output plots - ISMO's ----
  
  output$manage.obj.escape.1 <- renderPlot({
    
    ggplot(ind.stock_data(), aes(x = OriginDate)) +
      
      geom_line(aes(y = LowerSEG), color ="steelblue", linewidth = 0.7) +
      geom_ribbon(aes(ymin = LowerSEG_se_lwr, ymax = LowerSEG_se_upr), fill = "steelblue", alpha = 0.3) +
      
      geom_line(aes(y = UpperSEG), color ="steelblue", linewidth = 0.7) +
      geom_ribbon(aes(ymin = UpperSEG_se_lwr, ymax = UpperSEG_se_upr), fill = "steelblue", alpha = 0.3) +
      
      geom_ribbon(aes(ymin = LowerSEG_se_upr, ymax = UpperSEG_se_lwr), fill = "steelblue", alpha = 0.1) +
      
      geom_line(data = example.year.ind.stock_data(), aes(x = as.Date(JulianDay, origin = as.Date("0000-01-01")), y = SumEscape),
                color = "black", lty = 6, linewidth = 1, alpha = 0.5) +
      geom_point(data = example.year.ind.stock_data(), aes(x = as.Date(JulianDay, origin = as.Date("0000-01-01")), y = SumEscape),
                 color = "black", shape = 13, size = 3) +
      geom_line(data = ind.stock.CY_data(), aes(x = as.Date(JulianDay, origin = as.Date("0000-01-01")), y = SumEscape, color = SurveyType, lty = SurveyType),
                linewidth = 1, alpha = 0.5) +
      geom_point(data = ind.stock.CY_data(), aes(x = as.Date(JulianDay, origin = as.Date("0000-01-01")), y = SumEscape, fill = SurveyType, shape = SurveyType),
                 color = "black", size = 3) +
      geom_label_repel(data = ind.stock.CY_data(), aes(x = as.Date(JulianDay, origin = as.Date("0000-01-01")), y = SumEscape, label = Stream_Condition, size = 7, fontface = "bold"),
                 point.padding = 1.5, label.size = 0.8, color = "black", fill = "white", show.legend = FALSE) +
      scale_color_manual(values = c("red","blue","gold")) +
      scale_fill_manual(values = c("red","blue","gold")) + 
      scale_shape_manual(values = c(21,22,24)) +
      scale_linetype_manual(values = c(2,3,4)) +

      scale_x_date(breaks = "1 week", date_labels = "%b %d", limits = c(as.Date(121, origin = as.Date("0000-01-01")),as.Date(296, origin = as.Date("0000-01-01"))), expand = c(0.02,0.02)) +
      xlab("Date") +
      ylab("Cumulative Escapement") +
      labs(shape = "Survey Type", linetype = "Survey Type", color = "Survey Type", fill = "Survey Type", size = "Year") +
      
      theme(legend.position = "inside",
            legend.margin = margin(0, 0, 0, 0), # turned off for alignment
            legend.justification.inside = c(0.02,1),
            legend.box.background = element_rect(color = "black", linewidth = 1, linetype = 1),
            legend.box.margin = margin(5, 5, 5, 5))
    
  })
  
  output$manage.obj.escape.2 <- renderPlot({
    
    ggplot(data = ind.stock.CY_data(), aes(x = as.Date(JulianDay, origin = as.Date("0000-01-01")), y=SurveyCount, label = Stream_Condition, color = SurveyType, fill = SurveyType, shape = SurveyType, linetype = SurveyType)) + ## AUC plot
      
      geom_line(linewidth = 1, alpha = 0.5) + 
      geom_point(color = "black", size = 3) +
      geom_label_repel(aes(size = 7, fontface = "bold"), point.padding = 1.5, label.size = 0.8, color = "black", fill = "white", show.legend = FALSE) +
      scale_color_manual(values = c("red","blue","gold")) +
      scale_fill_manual(values = c("red","blue","gold")) + 
      scale_shape_manual(values = c(21,22,24)) +
      scale_linetype_manual(values = c(2,3,4)) +
      
      scale_x_date(breaks = "1 week", date_labels = "%b %d", limits = c(as.Date(121, origin = as.Date("0000-01-01")),as.Date(296, origin = as.Date("0000-01-01"))), expand = c(0.02,0.02)) +
      xlab("Date") +
      ylab("Survey Count") +
      labs(shape = "Survey Type", linetype = "Survey Type", color = "Survey Type", fill = "Survey Type") +
      
      theme(legend.position = "inside",
            legend.margin = margin(0, 0, 0, 0), # turned off for alignment
            legend.justification.inside = c(0.02,1),
            legend.box.background = element_rect(color = "black", linewidth = 1, linetype = 1),
            legend.box.margin = margin(5, 5, 5, 5))
    
  })
  
  output$manage.obj.escape.table <- renderDataTable(ind.stock.CY_table())
  
  ## Hover text around cursor on plot ----
  
  output$plot_info1 <- renderText({
    req(aggregate_data())
    
    paste0("Date = ", format(as.Date(input$plot_hover1$x), "%b %d"), 
           "\nCumulative escapement = ", number(round(as.numeric(input$plot_hover1$y),0), big.mark = ","),
           "\nHistorical run-timing average escapement-to-date %: ",100*filter(aggregate_data(),JulianDay == yday(as.Date(input$plot_hover1$x)))$PercentEscape,"%",
           "\nEstimated end-of-year escapement: ",number(round(as.numeric(input$plot_hover1$y)/filter(aggregate_data(),JulianDay == yday(as.Date(input$plot_hover1$x)))$PercentEscape,0), big.mark = ","))
  })
  
  output$plot_info2 <- renderText({
    paste0("Date = ", format(as.Date(input$plot_hover2$x), "%b %d"), 
           "\nSurvey Count = ", round(as.numeric(input$plot_hover2$y),0))
  })
  
  output$plot_info3 <- renderText({
    paste0("Date = ", format(as.Date(input$plot_hover3$x), "%b %d"), 
           "\nCumulative escapement = ", number(round(as.numeric(input$plot_hover3$y),0), big.mark = ","),
           "\nHistorical run-timing average escapement-to-date %: ",100*filter(ind.stock_data(),JulianDay == yday(as.Date(input$plot_hover3$x)))$PercentEscape,"%",
           "\nEstimated end-of-year escapement: ",number(round(as.numeric(input$plot_hover3$y)/filter(ind.stock_data(),JulianDay == yday(as.Date(input$plot_hover3$x)))$PercentEscape,0), big.mark = ","))
  })
  
  output$plot_info4 <- renderText({
    paste0("Date = ", format(as.Date(input$plot_hover4$x), "%b %d"), 
           "\nSurvey Count = ", round(as.numeric(input$plot_hover4$y),0))
  })

}


shinyApp(ui,server)
