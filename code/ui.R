
# getwd() # Adjust as needed to "inseason-escapement" directory

library(shiny)
library(shinythemes)
library(waiter)
library(DT)

ui <- fluidPage(
  useWaiter(), 
  waiterPreloader(), #optional loading screen that only shows briefly
  theme = shinytheme("sandstone"),
  navbarPage(paste(CurrentYear,"In-Season Run Timing Curve Visualization"), 
             tabPanel("Charts",
                      sidebarLayout(
                        sidebarPanel(
                          width = 2,
                          # tags$head(tags$style(HTML("hr {border-top: 1px solid #b3b3b3;}"))),
                          h2("Chart Options"),
                          hr(),
                          conditionalPanel(condition = "input.mainpanels_id == 'District-Wide Aggregate SEG Goals'",
                                           selectInput(inputId = "speciesInput1",
                                                       label = h3("Species"),
                                                       choices = list("Pink Salmon" = "Pink Salmon",
                                                                      "Chum Salmon" = "Chum Salmon")),
                                           selectInput(inputId = "districtInput1",
                                                       label = h3("District"),
                                                       c("")),
                                           selectInput(inputId = "yearInput1",
                                                       label = h3("Year Range"),
                                                       c(""))),
                          conditionalPanel(condition = "input.mainpanels_id == 'Individual-Stock Management Objective Goals'",
                                  selectInput(inputId = "speciesInput2",
                                              label = h3("Species"),
                                              choices = list("Pink Salmon" = "Pink Salmon",
                                                             "Chum Salmon" = "Chum Salmon",
                                                             "Sockeye Salmon" = "Sockeye Salmon")),
                                  selectInput(inputId = "stockInput2",
                                              label = h3("Stock"),
                                              c("")),
                                  selectInput(inputId = "yearInput2",
                                              label = h3("Year Range"),
                                              c("")))),
                        mainPanel(
                          width = 10,
                          tabsetPanel(
                            tabPanel(title = "District-Wide Aggregate SEG Goals", 
                                     plotOutput("agg.seg.escape", height = "700px", hover = hoverOpts(id ="plot_hover")),
                                     verbatimTextOutput("hover_info"),
                                     DTOutput("agg.seg.escape.table")),
                            tabPanel(title = "Individual-Stock Management Objective Goals", 
                                     plotOutput("manage.obj.escape", height = "700px", hover = hoverOpts(id ="plot_hover")),
                                     verbatimTextOutput("hover_info"),
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

  