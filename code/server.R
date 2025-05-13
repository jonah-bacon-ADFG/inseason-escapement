
# getwd() # Adjust as needed to "inseason-escapement" directory

library(tidyverse)
library(scales)
library(shiny)
library(shinythemes)
library(waiter)
library(DT)
library(gridExtra)
library(rsconnect)

CurrentYear <- 2023                                                             ## current in-season year 
StreamLife <- 17.5                                                              ## estimated life in stream of fish
ObsEff <- 1.0                                                                   ## observer efficiency

# Run two 'source' scripts below to load data. Alternatively, run code from 'code/helpers.R' and 'code/ui.R' 
# scripts individually first before running this script:
source('code/helpers.R')
source('code/ui.R')

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

server <- function(input, output, session) {
  
## Dynamic drop-down lists - SEG's ----
  
  observeEvent(input$speciesInput1,
               {
                 a <- input$speciesInput1
                 updateSelectInput(session,
                                   inputId = "districtInput1",
                                   choices = unique(RTC.AGG.df[(RTC.AGG.df$Species == input$speciesInput1 & RTC.AGG.df$TYPE.STOCK == "Aggregate"), ]$District)
                 )
               }
  )
  observeEvent(input$districtInput1,
               {
                 b <- input$districtInput1
                 updateSelectInput(session, "yearInput1",
                                   choices = unique(RTC.AGG.df[(RTC.AGG.df$Species == input$speciesInput1 & RTC.AGG.df$District == input$districtInput1), ]$YEAR.RANGE)
                 )
               }
  )

## Make data reactive - SEG's ----
  
  aggregate_data <- reactive({

    RTC.AGG.df %>% filter(Species == input$speciesInput1 & District == input$districtInput1 & YEAR.RANGE == input$yearInput1)

  })

  aggregate.CY_data <- reactive({

    AGG.CY.AUC.df %>%
      filter(SPECIES == input$speciesInput1 & DISTRICT == input$districtInput1)

  })
  
  aggregate.CY.surv_data <- reactive({
    
    AGG.CY.AUC.surv.df %>% 
      filter(SPECIES == input$speciesInput1 & DISTRICT == input$districtInput1)
    
  })
  
  aggregate.CY_table <- reactive({
    
    AGG.CY.AUC.table %>% 
      filter(SPECIES == input$speciesInput1 & DISTRICT == input$districtInput1) %>% 
      select(-c(SPECIES,DISTRICT))
    
  })
  
## Output plots - SEG's ----
  
  output$agg.seg.escape <- renderPlot({

p1 <- ggplot(aggregate_data(), aes(x = OriginDate)) +

      geom_line(aes(y = LowerSEG), color ="steelblue", linewidth = 0.7) +
      geom_ribbon(aes(ymin = LowerSEG_se_lwr, ymax = LowerSEG_se_upr), fill = "steelblue", alpha = 0.3) +

      geom_line(aes(y = UpperSEG), color ="steelblue", linewidth = 0.7) +
      geom_ribbon(aes(ymin = UpperSEG_se_lwr, ymax = UpperSEG_se_upr), fill = "steelblue", alpha = 0.3) +

      geom_ribbon(aes(ymin = LowerSEG_se_upr, ymax = UpperSEG_se_lwr), fill = "steelblue", alpha = 0.1) +

      geom_line(data = aggregate.CY_data(), aes(x = as.Date(JulianDay, origin = as.Date("0000-01-01")), y = DistSumEscape),
                color = "red", lty = 2, linewidth = 1, alpha = 0.5) +
      geom_point(data = aggregate.CY_data(), aes(x = as.Date(JulianDay, origin = as.Date("0000-01-01")), y = DistSumEscape),
                 shape = 25, fill = "red", color = "black", size = 3) +

      scale_x_date(breaks = "1 week", date_labels = "%b %d") +
      xlab("Date") +
      ylab("Cumulative Escapement")


    p2 <- ggplot(data = aggregate.CY.surv_data(), aes(x = as.Date(JulianDay, origin = as.Date("0000-01-01")), y=COUNT, color = STOCK, fill = STOCK)) + ## AUC plot

      geom_line(linewidth = 1, alpha = 0.5) +
      geom_point(shape = 25, color = "black", size = 3) +

      scale_x_date(breaks = "1 week", date_labels = "%b %d", limits = c(as.Date(121, origin = as.Date("0000-01-01")),as.Date(325, origin = as.Date("0000-01-01")))) +
      xlab("Date") +
      ylab("Survey Count") +
      
      theme(legend.position = "inside",
            legend.position.inside = c(0.1,0.9))

    grid.arrange(p1, p2, ncol = 1, nrow = 2)

  })

  output$agg.seg.escape.table <- renderDataTable(aggregate.CY_table())

## Dynamic drop-down lists - ISMO's ----
  
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

## Make data reactive - ISMO's ----
  
  ind.stock_data <- reactive({
    
    RTC.IND.df %>% filter(Species == input$speciesInput2 & Stock == input$stockInput2 & YEAR.RANGE == input$yearInput2)
    
  })
  
  ind.stock.CY_data <- reactive({
    
    IND.CY.AUC.df %>% 
      filter(SPECIES == input$speciesInput2 & STOCK == input$stockInput2)
    
  })
  
  ind.stock.CY_table <- reactive({
    
    IND.CY.AUC.table %>% 
      filter(SPECIES == input$speciesInput2 & Stock == input$stockInput2) %>% 
      select(-c(SPECIES,Stock))
    
  })

## Output plots - ISMO's ----
  
  output$manage.obj.escape <- renderPlot({
    
    p1 <- ggplot(ind.stock_data(), aes(x = OriginDate)) +
      
      geom_line(aes(y = LowerSEG), color ="steelblue", linewidth = 0.7) +
      geom_ribbon(aes(ymin = LowerSEG_se_lwr, ymax = LowerSEG_se_upr), fill = "steelblue", alpha = 0.3) +
      
      geom_line(aes(y = UpperSEG), color ="steelblue", linewidth = 0.7) +
      geom_ribbon(aes(ymin = UpperSEG_se_lwr, ymax = UpperSEG_se_upr), fill = "steelblue", alpha = 0.3) +
      
      geom_ribbon(aes(ymin = LowerSEG_se_upr, ymax = UpperSEG_se_lwr), fill = "steelblue", alpha = 0.1) +
      
      geom_line(data = ind.stock.CY_data(), aes(x = as.Date(JulianDay, origin = as.Date("0000-01-01")), y = SumEscape), 
                color = "red", lty = 2, linewidth = 1, alpha = 0.5) +
      geom_point(data = ind.stock.CY_data(), aes(x = as.Date(JulianDay, origin = as.Date("0000-01-01")), y = SumEscape), 
                 shape = 25, fill = "red", color = "black", size = 3) +
      
      scale_x_date(breaks = "1 week", date_labels = "%b %d") +
      xlab("Date") +
      ylab("Cumulative Escapement")
    
    
    p2 <- ggplot(data = ind.stock.CY_data(), aes(x = as.Date(JulianDay, origin = as.Date("0000-01-01")), y=COUNT)) + ## AUC plot
      
      geom_line(color = "red", linewidth = 1, alpha = 0.5) + 
      geom_point(shape = 25, fill = "red", color = "black", size = 3) +
      
      scale_x_date(breaks = "1 week", date_labels = "%b %d", limits = c(as.Date(121, origin = as.Date("0000-01-01")),as.Date(325, origin = as.Date("0000-01-01")))) +
      xlab("Date") +
      ylab("Survey Count")
    
    grid.arrange(p1, p2, ncol = 1, nrow = 2)

  })
  
  output$manage.obj.escape.table <- renderDataTable(ind.stock.CY_table())
  
## WORK IN PROGRESS: Hover text around cursor on plot ----
  
  # displayed_text <- reactive({
  #   req(input$plot_hover)
  #   hover <- input$plot_hover
  #   dist <- sqrt((hover$y - filtered_data2()$SumEscape)^2)
  # 
  #   if(min(dist) < 100) {
  #     filtered_data2()[filtered_data2()$SumEscape == which.min(dist),]
  #     filtered_data2()$SumEscape[which.min(dist)]
  #   } else {
  #     NULL
  #   }
  # })
  # 
  # output$hover_info <- renderPrint({
  #   req(displayed_text())
  # 
  #   cat("Cumulative escapement\n")
  #   displayed_text()
  # })
  
}

# shinyApp(ui,server)

# runApp(paste0(getwd(),"/code"))

# deployApp("code",
#           appName = "inseason_escapement_monitoring_app",
#           appTitle = "In-Season LCI Escapement Monitoring App",
#           launch.browser = FALSE
#           # appVisibility = "public"
# )