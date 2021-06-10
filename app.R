# MEA Visualiser Take 3
# Modularise some parts of the app code so to reduce the load and confusion 
# with shiny object identifiers.

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

# Source Modules here
source("modules.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "MEA Visualiser v3"),
    dashboardSidebar(
        sidebarMenu(
            menuItem(
                "Upload Files", tabName = "dataIn", icon = icon("database"),
                menuSubItem(
                    "Upload Raw MEA Files", tabName = "dataIn", icon = icon("database")
                ),
                # menuSubItem(
                #     "Upload Multiple MEA Files", tabName = "dataInMulti", icon = icon("database")
                #     ),
                menuSubItem(
                    "Preview Data Tables", tabName = "loadedTables", icon = icon("table")
                ),
                menuSubItem(
                    "Preview Spike Data Tables", tabName = "spikeTables", icon = icon("table")
                ),
                menuSubItem(
                    "Preview Spike Calculated Table", tabName = "spikeTablesMean", icon = icon("table")
                ) 
            )
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "dataIn",
                    fluidRow(box(fileInput('file', 'Load Treatment MEA File (UTF-8 .csv file)',
                                           accept = c(
                                               'text/csv',
                                               'text/comma-separated-values',
                                               '.csv'
                                           )),
                                 fileInput('file.baseline', 'Load Baseline MEA File (UTF-8 .csv file)',
                                           accept = c(
                                               'text/csv',
                                               'text/comma-separated-values',
                                               '.csv'
                                           )),
                                 fileInput('spike.test', 'Load Treatment Spike Data (.zip file, compressed csv)',
                                           accept = '.zip'),
                                 fileInput('spike.baseline', 'Load Baseline Spike Data (.zip file, compressed csv)',
                                           accept = '.zip')
                    ),
                    box(title = "Formatting and Filtering",
                        checkboxInput('rm.inactive', 'Remove inactive channels - Test', FALSE),
                        sliderInput(inputId = "sp.level",
                                    label = "Minimum Spike Rate Threshold (Hz) - Test :",
                                    min = 0,
                                    max = 10,
                                    value = 1),
                        checkboxInput('rm.inactive.base', 'Remove inactive channels - Baseline', FALSE),
                        sliderInput(inputId = "sp.level.baseline",
                                    label = "Minimum Spike Rate Threshold (Hz) - Baseline :",
                                    min = 0,
                                    max = 10,
                                    value = 1),
                        actionButton("choice", "Click To Import Data")
                    )
                    )
                    
            ),
            tabItem(tabName = "loadedTables",
                    title = "Tables Loaded and Ready to Plot",
                    fluidRow(h3("Test - MEA file"), displayDT_ui("test")),
                    fluidRow(h3("Baseline - MEA file"), displayDT_ui("base"))
            ),
            tabItem(tabName = "spikeTables",
                    title = "Spike Data Tables Loaded",
                    fluidRow(h3("Test Spike Data"), displayDT_ui("s.test")),
                    fluidRow(h3("Baseline Spike Data"), displayDT_ui("s.base"))
            ),
            tabItem(tabName = "spikeTablesMean",
                    title = "Calculated Means from Spike Data",
                    fluidRow(h3("Summarised Spike Table"), displayDT_ui("s.table")),
                    fluidRow(h3("Scatter Table"), DTOutput("scatTable")),
                    fluidRow(h3("Well Means Table"), DTOutput("wellScatTable"))
            )
        )
    )
)

# Define server logic required to draw a histogram

server <- function(input, output, session) {
    ###############################################################################
    options(shiny.maxRequestSize=30*1024^2)
    
    ##############################################################################  
    info <- eventReactive(input$choice, {
        inFile <- input$file
        req(inFile)
        f <- read_csv(inFile$datapath)
        f <- f %>% 
            filter(`Dose Label` != "Control") %>% # remove this data reading from drug 2 
            mutate(plate = str_extract(Experiment, "_[0-9][0-9][0-9][0-9]_")) %>% #Extract plate ID
            mutate(plate = as.numeric(gsub("_", "", plate))) %>% # reformat to numeric
            mutate(key = paste0(`Channel ID`, "_",
                                plate),
                   key2 = paste0(`Channel ID`, "_",
                                 `Dose Label`, "_", # need this for PCA
                                 plate),
                   key3 = paste0(plate, "_", `Well ID`, "_", `Dose Label`))
        
        ifelse(input$rm.inactive==TRUE,
               return(f %>% filter(`Spike Rate [Hz]` >= input$sp.level)),
               return(f))
    })
    
    ##############################################################################
    # This is the baseline table and is set to either include or remove inactive
    # channels
    info.base <- eventReactive(input$choice, {
        inFile <- input$file.baseline
        req(inFile)
        f <- read_csv(inFile$datapath)
        f <- f %>% 
            filter(`Dose Label` == "Control") %>% #select control data only
            mutate(plate = str_extract(Experiment, "_[0-9][0-9][0-9][0-9]_")) %>% #Extract plate ID
            mutate(plate = as.numeric(gsub("_", "", plate))) %>% # reformat to numeric
            mutate(key = paste0(`Channel ID`, "_",
                                plate),
                   key2 = paste0(`Channel ID`, "_",
                                 `Dose Label`, "_", # need this for PCA
                                 plate),
                   key3 = paste0(plate, "_", `Well ID`, "_", `Dose Label`)) #set up channel ID and experiment
        ifelse(input$rm.inactive.base==TRUE,
               return(f %>% filter(`Spike Rate [Hz]` >= input$sp.level.baseline)),
               return(f))
    })
    
    ##############################################################################
    # This is the baseline spike table
    # channels
    sp.base <- eventReactive(input$choice, {
        filter <- info.base()
        
        f <- read_csv(input$spike.baseline$datapath)
        f <- f %>% 
            filter(`Dose Label` == "Control") %>% #select control data only
            mutate(plate = str_extract(Experiment, "_[0-9][0-9][0-9][0-9]_")) %>% #Extract plate ID
            mutate(plate = as.numeric(gsub("_", "", plate))) %>% # reformat to numeric
            mutate(key = paste0(`Channel ID`, "_",
                                plate),
                   key2 = paste0(`Channel ID`, "_",
                                 `Dose Label`, "_", # need this for PCA
                                 plate),
                   key3 = paste0(plate, "_", `Well ID`, "_", `Dose Label`)) #set up channel ID and experiment
        ifelse(input$rm.inactive.base==TRUE,
               return(f %>% filter(key %in% filter$key)),
               return(f))
    })
    
    ##############################################################################
    # This is the baseline table and is set to either include or remove inactive
    # channels
    sp.test <- eventReactive(input$choice, {
        filter <- info.base()
        
        f <- read_csv(input$spike.test$datapath)
        f <- f %>% 
            filter(`Dose Label` != "Control") %>% #select measurement data only
            mutate(plate = str_extract(Experiment, "_[0-9][0-9][0-9][0-9]_")) %>% #Extract plate ID
            mutate(plate = as.numeric(gsub("_", "", plate))) %>% # reformat to numeric
            mutate(key = paste0(`Channel ID`, "_",
                                plate),
                   key2 = paste0(`Channel ID`, "_",
                                 `Dose Label`, "_", # need this for PCA
                                 plate),
                   key3 = paste0(plate, "_", `Well ID`, "_", `Dose Label`)) #set up channel ID and experiment
        ifelse(input$rm.inactive.base==TRUE,
               return(f %>% filter(key %in% filter$key)),
               return(f))
    })
    
    ##############################################################################
    # Make a cleaned and filtered data table for downstream analysis and plotting 
    # This is the raw spike and time course data across all channels and wells 
    # and it will contain only data that have been filtered for spike rate. 
    # 
    # Calculate the mean and variance min/max peak and peak to peak data for
    # inclusion into the PCA plots.
    # 
    spike.table <- eventReactive(input$choice, {
        scatter.test <- sp.test()
        ctrl <- sp.base()
        
        scatter.test <- scatter.test %>% 
            filter(key %in% ctrl$key) 
        
        scatter.test <- bind_rows(scatter.test, ctrl)
        
        scatter.test <- scatter.test %>% 
            group_by(key2) %>% 
            summarize(mean_maxAmp_pV = mean(`Maximum Amplitude [pV]`), var_maxAmp_pV = var(`Maximum Amplitude [pV]`),
                      mean_minAmp_pV = mean(`Minimum Amplitude [pV]`), var_minAmp_pV = var(`Minimum Amplitude [pV]`),
                      mean_peakToPeak_pV = mean(`Peak-to-peak Amplitude [pV]`), 
                      var_peakToPeak_pV = var(`Peak-to-peak Amplitude [pV]`)) %>% 
            tibble()
        
    })
    
    ##############################################################################
    # Make a cleaned and filtered data table for downstream analysis and plotting 
    # This will collect and apply the filters for spike rate between the two 
    # data tables and then be used to present and work with plotting
    # 
    scat.table <- eventReactive(input$choice, {
        scatter.test <- info()
        ctrl <- info.base()
        spike.test <- spike.table()
        
        scatter.test <- scatter.test %>% 
            filter(key %in% ctrl$key) 
        
        scatter.test <- bind_rows(scatter.test, ctrl)
        
        scatter.test <- left_join(scatter.test, spike.test, by = "key2")
        
        scatter.test <- scatter.test %>% 
            dplyr::select(`Channel ID`:`Dose Label`, plate:key3, `Spike Count`:`Mean Network Interburst Interval [µs]`,
                          mean_maxAmp_pV:var_peakToPeak_pV)
        
        vars2 <- names(scatter.test)
        vars3 <- names(scatter.test)
        updateSelectInput(session, "measurement","Select Measurement Column (y)", choices = vars2, selected = "Burst Count")
        updateSelectInput(session, "measurement2","Select Measurement Column (y)", choices = vars2, selected = "Burst Count")
        updateSelectInput(session, "sampleid","Select Group Column (x)", choices = vars3, selected = "Compound ID")
        updateSelectInput(session, "sampleid2","Select Group of Interest", choices = vars3, selected = "Compound ID")
        updateSelectInput(session, "sampleid3","Select Group Column (x)", choices = vars3, selected = "Compound ID")
        updateSelectInput(session, "sampleid4","Select Group of Interest", choices = vars3, selected = "Compound ID")
        
        scatter.test
        
        
    })
    
    ##############################################################################
    # Make a summarised table from the scatter.test which is by channel aggregate
    # by well and plate here to calculate the mean and variance of each well 
    # within the experiment then this will feed into new scatter plots where only
    # datapoints by well are presented
    
    well.scat <- reactive({
        dat <- scat.table()
        
        dat <- dat %>% 
            group_by(key3,`Dose Label`, `Compound ID`, plate) %>% 
            summarise(across(`Spike Count`:var_peakToPeak_pV, list(mean = mean))) %>% 
            tibble()
        colnames(dat) <- gsub("_mean", "", colnames(dat))
        dat
    })
    
    ##############################################################################
    # With the uber dataset, now create the data structure for plotting of the 
    # scatter and PCA's after filtering
    uber.table <- eventReactive(input$plotScatter, {
        scatter.test <- scat.table()
        
        scatter.test <- scatter.test %>% 
            filter(`Dose Label` %in% input$dose.label.select) %>% 
            filter(!!rlang::sym(input$sampleid) %in% input$sample.order) %>% 
            mutate(!!rlang::sym(input$sampleid) := factor(!!rlang::sym(input$sampleid), levels = input$sample.order))
    })
    
    ##############################################################################
    # With the uber dataset, now create the data structure for plotting of the 
    # scatter and PCA's after filtering
    well.uber <- eventReactive(input$w.plotScatter, {
        scatter.test <- well.scat()
        
        scatter.test <- scatter.test %>% 
            filter(`Dose Label` %in% input$w.dose.label.select) %>% 
            filter(!!rlang::sym(input$sampleid3) %in% input$w.sample.order) %>% 
            mutate(!!rlang::sym(input$sampleid3) := factor(!!rlang::sym(input$sampleid3), levels = input$w.sample.order))
    })
    
    ############################################################################## 
    # Lets try and modularise this for output into the UI
    #
    displayDT_server(id ="test", dat = info())
    displayDT_server(id ="base", dat = info.base())
    displayDT_server(id ="s.base", dat = sp.base())
    displayDT_server(id ="s.test", dat = sp.test())
    displayDT_server(id ="s.table", dat = spike.table())

    
}

# Run the application 
shinyApp(ui = ui, server = server)
