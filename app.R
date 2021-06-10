# MEA Visualiser Take 3
# Modularise some parts of the app code so to reduce the load and confusion 
# with shiny object identifiers.

# Define Modules here




library(shiny)
library(tidyverse)
library(DT)
library(ggbeeswarm)
library(PCAtools)
library(ggpubr)
library(rstatix)
library(ggalt)
library(plotly)
library(shinydashboard)
library(colourpicker)

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "MEA Visualiser"),
    dashboardSidebar(
        sidebarMenu(
            menuItem(
                "Upload Files", tabName = "dataIn", icon = icon("database"),
                menuSubItem(
                    "Upload Raw MEA Files", tabName = "dataIn", icon = icon("database")
                ),
                menuSubItem(
                    "Preview Data Tables", tabName = "loadedTables", icon = icon("table")
                ),
                menuSubItem(
                    "Preview Spike Data Tables", tabName = "spikeTables", icon = icon("table")
                ),
                menuSubItem(
                    "Preview Spike Calculated Table", tabName = "spikeTablesMean", icon = icon("table")
                )
            ),
            menuItem(
                "Plots - By Channel", tabName = "plots", icon = icon("chart-bar")
            )
        ),
        dashboardBody(
            tabItems(
                tabItem(
                    tabName = "dataIn",
                    fluidRow(
                        box(
                            fileInput(
                                'file', 
                                'Load Treatment MEA File (UTF-8 .csv file)',
                                accept = c('text/csv',
                                           'text/comma-separated-values',
                                           '.csv')
                            ),
                            fileInput(
                                'file.baseline', 
                                'Load Baseline MEA File (UTF-8 .csv file)',
                                accept = c('text/csv',
                                           'text/comma-separated-values',
                                           '.csv')
                            ),
                            fileInput(
                                'spike.test', 
                                'Load Treatment Spike Data (.zip file, compressed csv)',
                                accept = '.zip'
                            ),
                            fileInput(
                                'spike.baseline', 
                                'Load Baseline Spike Data (.zip file, compressed csv)',
                                accept = '.zip'
                            )
                        ),
                        box(
                            title = "Formatting and Filtering",
                            checkboxInput(
                                'rm.inactive',
                                'Remove inactive channels - Test', 
                                FALSE
                            ),
                            sliderInput(
                                inputId = "sp.level",
                                label = "Minimum Spike Rate Threshold (Hz) - Test :",
                                min = 0,
                                max = 10,
                                value = 1
                            ),
                            checkboxInput(
                                'rm.inactive.base', 
                                'Remove inactive channels - Baseline', 
                                FALSE
                            ),
                            sliderInput(
                                inputId = "sp.level.baseline",
                                label = "Minimum Spike Rate Threshold (Hz) - Baseline :",
                                min = 0,
                                max = 10,
                                value = 1
                            ),
                            actionButton(
                                "choice", 
                                "Click To Import Data"
                            )
                        )
                    )
                ),
                tabItem(
                    tabName = "loadedTables",
                    fluidRow(
                        h3("Test - MEA file"), 
                        DTOutput("table_display")
                    ),
                    fluidRow(
                        h3("Baseline - MEA file"), 
                        DTOutput("table_display_base")
                    )
                ),
                tabItem(
                    tabName = "spikeTables",
                    fluidRow(
                        h3("Test Spike Data"), 
                        DTOutput("sp_test_out")
                    ),
                    fluidRow(
                        h3("Baseline Spike Data"), 
                        DTOutput("sp_baseline_out")
                    )
                ),
                tabItem(
                    tabName = "spikeTablesMean",
                    title = "Calculated Means from Spike Data",
                    fluidRow(
                        h3("Summarised Spike Table"), 
                        DTOutput("sp_means")
                    ),
                    fluidRow(
                        h3("Scatter Table"), 
                        DTOutput("scatTable")
                    ),
                    fluidRow(
                        h3("Well Means Table"), 
                        DTOutput("wellScatTable")
                    )
                )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
