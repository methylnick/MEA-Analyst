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
source("helperFunctions.R")

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
            ),
            menuItem(
                "Plots - By Channel", tabName = "plots", icon = icon("chart-bar"),
                menuSubItem(
                    "Histogram Plots", tabName = "plots", icon = icon("chart-bar")
                ),
                menuSubItem(
                    "Scatter Plots", tabName = "scatPlots", icon = icon("braille")
                ),
                menuSubItem(
                    "PCA Plots", tabName = "pcaPlots", icon = icon("blackberry")
                ),
                menuSubItem(
                    "Epilepiform Plots (PCA)", tabName = "epiPlots", icon = icon("user")
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
                    fluidRow(h3("Table for Scatter Plots"), displayDT_ui("sc.table")),
                    fluidRow(h3("Selected Table from Scatter Interface"), displayDT_ui("u.table"))
            ),
            tabItem(tabName = "plots",
                    title = "Histogram Plots",
                    fluidRow(box(title = "Histogram Options",
                                 extractMeasurementColumns_UI("histCols"),
                                 sliderInput(inputId = "bins",
                                             label = "Histogram - No# of bins :",
                                             min = 1,
                                             max = 50,
                                             value = 30)),
                             box(plotOutput(outputId = "distPlot"))),
                    fluidRow(box(plotOutput(outputId = "distPlot2")),
                             box(plotOutput(outputId = "distPlot3"))
                    )
            ),
            tabItem(tabName = "scatPlots",
                    outputId = "scatter_box_text",
                    fluidRow(box(title = "Scatter Plot Configuration",
                                 extractScatter_UI("channelScatter"),
                                 extractMeasurementColumns_UI("colsScatter"),
                                 actionButton("plotScatter", "Select Groups and Plot")
                    ),
                    box(title = "Tweaks to the Charts",
                        sliderInput(inputId = "point.size",step = 0.5,
                                    label = "Point Size :",
                                    min = 0,
                                    max = 8,
                                    value = 3),
                        sliderInput(inputId = "box.width",step = 0.05,
                                    label = "Box Width :",
                                    min = 0.05,
                                    max = 0.8,
                                    value = 0.5),
                        sliderInput(inputId = "ylim.rel",step = 100,
                                    label = "y limit %Rel :",
                                    min = 200,
                                    max = 2000,
                                    value = 800),
                        sliderInput(inputId = "text.size",step = 1,
                                    label = "Text Size :",
                                    min = 0,
                                    max = 30,
                                    value = 6),
                        downloadButton("downloadData", "Download Normalised Data")
                    )
                    
                    ),
                    fluidRow(box(title = " - Raw Data", plotlyOutput(outputId = "scatter_box")),
                             box(title = " - Relative to Baseline ( % ) - 'jitter'", plotlyOutput(outputId = "scatter_box_rel"))
                    ),
                    fluidRow(box(title =" - Relative to Baseline ( % ) - 'beeswarm'", plotlyOutput(outputId = "swarm_box_rel")),
                             box(displayDT_ui("n.u.table"))
                    )
                    
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
    displayDT_server(id ="u.table", dat = uber.table())
    displayDT_server(id ="sc.table", dat = scat.table())
    displayDT_server(id ="n.u.table", dat = norm.uber.table())

    ############################################################################## 
    # Lets try and modularise measurement column extraction for selection in 
    # graphs then extract out the value for plotting
    hcols <- extractMeasurementColumns_server(id = "histCols", dat = scat.table())
    
    
    ##############################################################################
    output$distPlot <- renderPlot({
        f <- scat.table()
        f %>% 
            select(!!rlang::sym(hcols())) %>% 
            drop_na() %>% 
            ggplot(aes(x = !!rlang::sym(hcols()))) +
            geom_histogram(bins = input$bins) +
            ggtitle(paste0("Histogram - ",hcols())) +
            theme(axis.title.x = element_blank())
    })
    
    ##############################################################################
    output$distPlot2 <- renderPlot({
        f <- scat.table()
        f %>% 
            select(!!rlang::sym(hcols()), `Dose Label`) %>% 
            drop_na() %>% 
            ggplot(aes(x = !!rlang::sym(hcols()), colour = `Dose Label`)) +
            geom_histogram(bins = input$bins, fill = "white", position = "dodge") +
            ggtitle(paste0("Histogram - ",hcols())) +
            theme(axis.title.x = element_blank())
    })
    
    ##############################################################################
    output$distPlot3 <- renderPlot({
        f <- scat.table()
        f %>% 
            select(!!rlang::sym(hcols()), `Compound ID`) %>% 
            drop_na() %>% 
            ggplot(aes(x = !!rlang::sym(hcols()), colour = `Compound ID`)) +
            geom_histogram(bins = input$bins, fill = "white", position = "dodge") +
            ggtitle(paste0("Histogram - ",hcols())) +
            theme(axis.title.x = element_blank())  
    })
    
    ############################################################################## 
    # Modularise selection columns extraction for 
    # graphs then extract out the value for plotting
    scatterChannel <- extractScatter_server(id = "channelScatter", dat = scat.table())
    scatterCols <- extractMeasurementColumns_server(id = "colsScatter", dat = scat.table())
    
    ##############################################################################
    # With the uber dataset, now create the data structure for plotting of the 
    # scatter and PCA's after filtering
    uber.table <- eventReactive(input$plotScatter, {
        
        scatter.test <- scat.table() %>% 
            dplyr::filter(`Dose Label` == scatterChannel$doseLabelSelect()) %>% 
            dplyr::filter(`Compound ID` %in% scatterChannel$sampleOrder()) %>% 
            mutate(`Compound ID` := factor(`Compound ID`, levels = scatterChannel$sampleOrder()))
        
        return(scatter.test)
    })
    
    
    ##############################################################################
    # Create a plotly scatter object by channel
    output$scatter_box <- renderPlotly({
        ggplotly(
            uber.table() %>% 
                ggplot(aes(y = !!rlang::sym(scatterCols()), 
                           x = `Compound ID`,
                           col= `Compound ID`,
                           label = `Channel ID`,
                           label2 = `Channel Label`,
                           label3 = `Well ID`,
                           label4 = plate)) +
                geom_hline(yintercept = 0, alpha=0.5,linetype=2) +
                geom_jitter(position=position_jitter(width=0.3, height=0.2),size=input$point.size, alpha=0.9) +
                geom_boxplot(alpha = 0.5, show.legend = FALSE,col="black",width=input$box.width,lwd=0.8) +
                theme_classic() +
                labs(y=input$measurement) +
                theme(
                    axis.text = element_text(size = input$text.size,face = "bold"),
                    axis.title = element_text(size = input$text.size*1.3,face = "bold"),
                    axis.title.x = element_blank(),
                    legend.position = "none"
                )
        )
    })
    
    ##############################################################################
    # Create a normalised table of measurements from the uber table that has been
    # selected and filtered for the variables of interest. Calculate the normalised
    # values for all measures and columns of interest. 
    # 
    norm.uber.table <- eventReactive(input$plotScatter, {
        scatter.test <- scat.table()
        
        ctrl <- scatter.test %>% 
            filter(`Dose Label` == "Control")
        
        scatter.test <- left_join(ctrl, scatter.test, by = "key")
        
        colnames(scatter.test) <- gsub("\\.y", "", colnames(scatter.test))
        
        scatter.test <- scatter.test %>% 
            mutate(across(`Spike Count`:`Mean Network Interburst Interval [µs]`, 
                          ~ .x / scatter.test[[paste0(cur_column(), ".x")]] * 100)) %>% 
            select(-(`Spike Count.x`:`var_peakToPeak_pV.x`))
        
        scatter.test <- scatter.test %>% 
            dplyr::filter(`Dose Label` == scatterChannel$doseLabelSelect()) %>% 
            dplyr::filter(`Compound ID` %in% scatterChannel$sampleOrder()) %>% 
            mutate(`Compound ID` := factor(`Compound ID`, levels = scatterChannel$sampleOrder()))
        
    })
    
    ##############################################################################
    # This normalises the measurement values to the baseline data table does a merge 
    # on channel ID
    output$scatter_box_rel <- renderPlotly({
        ggplotly(
            norm.uber.table() %>% 
                ggplot(aes(y = !!rlang::sym(scatterCols()), 
                           x = `Compound ID`,
                           col = `Compound ID`,
                           label = `Channel ID`,
                           label2 = `Channel Label`,
                           label3 = `Well ID`,
                           label4 = plate)) +
                geom_hline(yintercept = 100, alpha=0.5,linetype=2) +
                geom_jitter(position=position_jitter(width=0.3, height=0.2),size=input$point.size, alpha=0.9) +
                geom_boxplot(alpha = 0.5, show.legend = FALSE,col="black",width=input$box.width,lwd=0.8) +
                theme_classic() +
                labs(y=paste0(input$measurement,"\n% Relative to Baseline\n")) +
                theme(axis.text = element_text(size = input$text.size,face = "bold"),
                      axis.title = element_text(size = input$text.size*1.3,face = "bold"),
                      axis.title.x = element_blank(),
                      legend.position = "none") +
                coord_cartesian(ylim=c(0,input$ylim.rel))
        )
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
