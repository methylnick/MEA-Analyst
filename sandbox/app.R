#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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

source("modules.R")
source("helperFunctions.R")

data <- read_csv("3659_TOP and TIAG_Drug 2_190321.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
           extractScatter_UI("channelScatter"),
           extractMeasurementColumns_UI("colsScatter"),
           actionButton("plotScatter", "Plot the scatter")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           displayDT_ui("u.table"),
           DTOutput("emitscatterChannel"),
           textOutput("textOut")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
    ############################################################################## 
    # Modularise selection columns extraction for 
    # graphs then extract out the value for plotting
    scatterChannel <- extractScatter_server(id = "channelScatter", dat = data)
    scatterCols <- extractMeasurementColumns_server(id = "colsScatter", dat = data)
    
    uber.table <- eventReactive(input$plotScatter, {
        
        scatter.test <- data %>% 
            dplyr::filter(`Dose Label` == scatterChannel$doseLabelSelect()) %>% 
            dplyr::filter(`Compound ID` %in% scatterChannel$sampleOrder()) %>% 
            mutate(`Compound ID` := factor(`Compound ID`, levels = scatterChannel$sampleOrder()))
        
        return(scatter.test)
    })
    
    output$textOut <- renderText({
        scatterChannel$doseLabelSelect()
    })
    
    output$emitscatterChannel <- renderDT({
        DT::datatable(uber.table(), options = list(scrollX = TRUE))
    })
    
    
    
    displayDT_server(id ="u.table", dat = scatterChannel$filtered_data)
}

# Run the application 
shinyApp(ui = ui, server = server)
